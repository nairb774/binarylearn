package org.no.ip.bca.superlearn

import java.lang.Math.exp

import java.util.concurrent.Callable

import org.no.ip.bca.scala.{ FastRandom, Ranges }

import math.{ Matrix, Vector }
import math.Types._
import math.Implicits._

class WorkerStep[L, R, M](
  m: MatrixD[L, R],
  bias: VectorD[R],
  iter: DataIterator[L, M],
  random: FastRandom) extends DataIterator[R, M] {

  assert(m.columns == bias.length)
  val size = iter.size
  val metaSize = iter.metaSize
  val out = Vector.withLength[Double, R](bias.length)

  val hasNext = iter.hasNext
  def next = {
    val bias = this.bias.v
    val random = this.random

    iter.next * (m, out) |< { (i, sum) => if (1.0 / (1.0 + exp(-sum - bias(i))) >= random.nextDouble()) 1.0 else 0.0 }
  }
  val meta = iter.meta
  val skip = iter.skip
}

class MatrixJob(
  source: MemMapSource,
  range: Ranges.Pair,
  state: State,
  config: ClientConfig) extends Callable[Compute] {

  @volatile
  private var _canceled = false
  def cancel = _canceled = true

  def call: Compute = {
    MatrixWorker.builder(source.iter(range)).worker(state, config)(_canceled)
  }
}

object MatrixWorker {
  private type AD = Array[Double]
  class Builder private[MatrixWorker] (iter: DataIterator[Side.V, Side.V], random: FastRandom) {
    def preStep(m: MatrixD[Side.V, Side.V], bias: VectorD[Side.V]): Builder = {
      new Builder(new WorkerStep(m, bias, iter, random), random)
    }
    def worker(state: State, config: ClientConfig) = {
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
  v0: VectorD[Side.V],
  h0: VectorD[Side.H],
  v1: VectorD[Side.V],
  h1: VectorD[Side.H],
  root: DataIterator[Side.H, Side.V],
  sample: Double,
  random: FastRandom) extends ((=> Boolean) => Compute) {
  private type AB = Array[Byte]
  private type AL = Array[Long]

  def apply(canceled: => Boolean): Compute = {
    val v0 = this.v0
    val h0 = this.h0
    val v1 = this.v1
    val h1 = this.h1

    val sample = this.sample

    val cd = Matrix.withSize[Double, Side.V, Side.H](v0.length, h0.length)
    
    val v1h1 = Matrix.withSize[Double, Side.V, Side.H](v1.length, h1.length)
    val v0h0 = Matrix.withSize[Double, Side.V, Side.H](v1.length, h1.length)
    val v1v0 = Vector.withLength[Double, Side.V](v1.length)
    val h1h0 = Vector.withLength[Double, Side.H](h1.length)

    val root = this.root
    val vAct = v0.empty
    val hAct = h0.empty
    var count: Long = 0
    while (root.hasNext()) {
      if (random.nextDouble < sample) {
        if (canceled) return null
        root.next
        cd +< ((v1 ^ (h1, v1h1)) -> (v0 ^ (h0, v0h0)))
        vAct +< (v1 - (v0, v1v0))
        hAct +< (h1 - (h0, h1h0))
        count += 1
      } else {
        root.skip()
      }
    }
    Compute(cd, vAct, hAct, count)
  }
}

import java.util.concurrent.FutureTask

class MatrixWorkerFuture(
  matrixWorker: MatrixJob,
  onDone: MatrixWorkerFuture => Unit) extends FutureTask(matrixWorker) {
  override def cancel(interrupt: Boolean) = {
    try {
      super.cancel(interrupt)
    } finally {
      matrixWorker.cancel
    }
  }
  override def done = {
    if (!isCancelled) onDone(this)
  }
}
