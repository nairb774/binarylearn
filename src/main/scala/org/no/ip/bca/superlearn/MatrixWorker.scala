package org.no.ip.bca.superlearn

import java.lang.Math.exp

import java.util.concurrent.Callable

import org.no.ip.bca.scala.{ FastRandom, Ranges }

class WorkerStep(
  m: Array[Double],
  bias: Array[Double],
  iter: DataIterator,
  random: FastRandom) extends DataIterator {
  private type AB = Array[Byte]
  private type AD = Array[Double]
  val out = new AB(bias.length)

  def hasNext = iter.hasNext
  def next = {
    val v = iter.next
    val m = this.m
    val bias = this.bias
    val random = this.random
    val out = this.out

    var i = 0
    var y = 0
    var x = 0
    var sum = 0.0
    val vLength = v.length
    val mLength = m.length
    while (i < mLength) {
      sum += m(i) * v(x)
      i += 1
      x += 1
      if (x == vLength) {
        sum = 1.0 / (1.0 + exp(-sum - bias(y)))
        out(y) = if (sum >= random.nextDouble()) 1 else 0
        y += 1
        x = 0
        sum = 0.0
      }
    }
    out
  }
  def skip = iter.skip
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
  class Builder private[MatrixWorker] (iter: DataIterator, random: FastRandom) {
    def preStep(m: AD, bias: AD): Builder = {
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

  def builder(iter: DataIterator) = new Builder(iter, new FastRandom)
}

class MatrixWorker(
  v0: Array[Byte],
  h0: Array[Byte],
  v1: Array[Byte],
  h1: Array[Byte],
  root: DataIterator,
  sample: Double,
  random: FastRandom) extends ((=> Boolean) => Compute) {
  private type AB = Array[Byte]
  private type AL = Array[Long]

  def apply(canceled: => Boolean): Compute = {
    val v0 = this.v0
    val h0 = this.h0
    val v1 = this.v1
    val h1 = this.h1

    val w = v0.length
    val h = h0.length

    val sample = this.sample

    val cd = new AL(w * h)

    val root = this.root
    val vAct = new AL(w)
    val hAct = new AL(h)
    var count: Long = 0
    while (root.hasNext) {
      if (random.nextDouble < sample) {
        if (canceled) return null
        root.next
        explode(v0, h0, v1, h1)(cd)
        mergeActivations(v0, v1)(vAct)
        mergeActivations(h0, h1)(hAct)
        count += 1
      } else {
        root.skip
      }
    }
    Compute(cd, vAct, hAct, count)
  }

  def explode(v0: AB, h0: AB, v1: AB, h1: AB)(m: AL) {
    var i = 0
    var y = 0
    var x = 0
    var h1y = h0(y)
    var h2y = h1(y)
    val v1Length = v0.length
    val mLength = m.length
    while (i < mLength) {
      if (x == v1Length) {
        y += 1
        x = 0
        h1y = h0(y)
        h2y = h1(y)
      }
      m(i) += v0(x) * h1y - v1(x) * h2y
      i += 1
      x += 1
    }
  }

  def mergeActivations(a: AB, b: AB)(out: AL) = {
    var i = 0
    val outLength = out.length
    while (i < outLength) {
      out(i) += a(i) - b(i)
      i += 1
    }
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
