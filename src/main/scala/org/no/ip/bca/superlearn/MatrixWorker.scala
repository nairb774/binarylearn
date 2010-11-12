package org.no.ip.bca.superlearn

import scala.math.exp

import org.no.ip.bca.scala.{ FastRandom }
import math.{ JBlasMatrix, Matrix }
import se.scalablesolutions.akka.actor.{ Actor, ActorRef }
import se.scalablesolutions.akka.dispatch.Dispatchers

class WorkerStep[L, R, M](
  m: Matrix.Mutable[L, R],
  bias: Matrix.MutableV[L],
  iter: DataIterator[R, M],
  random: FastRandom) extends DataIterator[L, M] {

  val size = iter.size
  val metaSize = iter.metaSize
  val out = Matrix.mutable.vector[L](bias.size)

  val hasNext = iter.hasNext
  def next = {
    val random = this.random

    m * (iter.next, out)
    out += bias
    out |= { sum => if (1.0 / (1.0 + exp(-sum)) >= random.nextDouble()) 1.0 else 0.0 }
  }
  val meta = iter.meta
  val skip = iter.skip
}

object MatrixWorker {
  private type AD = Array[Double]
  class Builder private[MatrixWorker] (iter: DataIterator[Side.V, Side.V], random: FastRandom) {
    def preStep(m: Matrix.Mutable[Side.V, Side.V], bias: Matrix.MutableV[Side.V]): Builder = {
      new Builder(new WorkerStep(m, bias, iter, random), random)
    }
    def worker(state: MutableState, config: ClientConfig) = {
      val h0Iter = new WorkerStep(state.weights, state.hidden, iter, random)

      var v1Iter = new WorkerStep(state.transposedWeights, state.visible, h0Iter, random)
      var h1Iter = new WorkerStep(state.weights, state.hidden, v1Iter, random)

      var i = config.steps
      while (i > 1) {
        v1Iter = new WorkerStep(state.transposedWeights, state.visible, h1Iter, random)
        h1Iter = new WorkerStep(state.weights, state.hidden, v1Iter, random)
        i -= 1
      }

      new MatrixWorker(iter.out, h0Iter.out, v1Iter.out, h1Iter.out, h1Iter, config.sample, random)
    }
  }

  def builder(iter: DataIterator[Side.V, Side.V]) = new Builder(iter, new FastRandom)
}

class MatrixWorker(
  v0: Matrix.MutableV[Side.V],
  h0: Matrix.MutableV[Side.H],
  val v1: Matrix.MutableV[Side.V],
  h1: Matrix.MutableV[Side.H],
  val root: DataIterator[Side.H, Side.V],
  sample: Double,
  random: FastRandom) extends ((=> Boolean) => Option[Compute]) {
  private type AB = Array[Byte]
  private type AL = Array[Long]

  def apply(canceled: => Boolean): Option[Compute] = {
    val v0 = this.v0
    val h0 = this.h0
    val v1 = this.v1
    val h1 = this.h1

    val sample = this.sample

    var cd = Matrix.mutable.matrix[Side.H, Side.V](h0.size, v0.size)
    var v0h0 = Matrix.mutable.matrix[Side.H, Side.V](h0.size, v0.size)
    var v1h1 = Matrix.mutable.matrix[Side.H, Side.V](h0.size, v0.size)

    val root = this.root
    var vAct = v0.zero
    var hAct = h0.zero
    var count: Long = 0
    while (root.hasNext()) {
      if (random.nextDouble < sample) {
        if (canceled) return None
        root.next
        /*
        println(v0)
        println(h0)
        println(v1)
        println(h1)
        println()
        */
        //cd += ((h0 * (v0.T, v0h0)) -= (h1 * (v1.T, v1h1)))
        JBlasMatrix.ger(1, h0, v0, cd)
        JBlasMatrix.ger(-1, h1, v1, cd)
        vAct += (v0 -= v1)
        hAct += (h0 -= h1)
        count += 1
      } else {
        root.skip()
      }
    }
    Some(Compute(cd.toImmutable, vAct.toImmutable, hAct.toImmutable, count))
  }
}

object WorkerActor {
  class Work(worker: MatrixWorker) extends (() => Option[Compute]) {
    @volatile
    private var canceled = false
    def cancel = canceled = true
    def apply = worker(canceled)
  }
  final case class Run(worker: Work)
}

class WorkerActor extends Actor {
  self.dispatcher = Dispatchers.newThreadBasedDispatcher(self)
  def receive = {
    case WorkerActor.Run(work) =>
      work() foreach { result =>
        val msg = ServerActor.RecieveCompute(result)
        self reply msg
        self.sender foreach { _ ! msg }
      }
  }
}
