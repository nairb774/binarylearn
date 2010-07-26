package org.no.ip.bca.superlearn

import java.lang.Math.exp

import org.no.ip.bca.scala.Ranges

class MatrixWorker(
    state: State,
    transposedWeights: Array[Double],
    range: Ranges.Pair,
    dataManager: DataManager) extends java.util.concurrent.Callable[Compute] {
  private type AD = Array[Double]
  private type AL = Array[Long]
  
  @volatile
  private var _canceled = false
  def cancel = _canceled = true
  private val random = new FastRandom
  
  def call: Compute = {
    val weights = state.weights
    val w = state.w
    val h = state.h
    val transposedWeights = this.transposedWeights
    val hiddenBias = state.hidden
    val visibleBias = state.visible
    
    val start = range.start
    val end = range.end
    val sample = state.sample
    val steps = state.steps
    
    val v0 = new AL(w)
    val h0 = new AL(h)
    val v1 = new AL(w)
    val h1 = new AL(h)
    
    val cd = new AL(w * h)
    
    val iter = dataManager.iter(start, end)
    val vAct = new AL(w)
    val hAct = new AL(h)
    var count: Long = 0
    while (iter.hasNext) {
      if (random.nextDouble < sample) {
        if (_canceled) return null
        toBinaryDoubleArray(iter.next, v0)
        mult(v0, weights, hiddenBias)(h0)
        mult(h0, transposedWeights, visibleBias)(v1)
        mult(v1, weights, hiddenBias)(h1)
        var i = steps
        while (i > 1) {
          mult(h1, transposedWeights, visibleBias)(v1)
          mult(v1, weights, hiddenBias)(h1)
          i -= 1
        }
        explode(v0, h0, v1, h1)(cd)
        mergeActivations(v0, v1)(vAct)
        mergeActivations(h0, h1)(hAct)
        count += 1
      } else {
        iter.skip
      }
    }
    Compute(cd, vAct, hAct, count)
  }
  
  def mult(v: AL, m: AD, bias: AD)(out: AL) = {
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
  }
  
  def explode(v1: AL, h1: AL, v2: AL, h2: AL)(m: AL) {
    var i = 0
    var y = 0
    var x = 0
    var h1y = h1(y)
    var h2y = h2(y)
    val v1Length = v1.length
    val mLength = m.length
    while (i < mLength) {
      if (x == v1Length) {
        y += 1
        x = 0
        h1y = h1(y)
        h2y = h2(y)
      }
      m(i) += v1(x) * h1y - v2(x) * h2y
      i += 1
      x += 1
    }
  }
  
  def mergeActivations(a: AL, b: AL)(out: AL) = {
    var i = 0
    val outLength = out.length
    while (i < outLength) {
      out(i) += a(i) - b(i)
      i += 1
    }
  }
  
  def toBinaryDoubleArray(src: Array[Byte], dest: AL) {
    var i = 0
    val destLength = dest.length
    while (i < destLength) {
      dest(i) = (src(i) & 1)
      i += 1
    }
  }
}

import java.util.concurrent.FutureTask

class MatrixWorkerFuture(
    matrixWorker: MatrixWorker,
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
