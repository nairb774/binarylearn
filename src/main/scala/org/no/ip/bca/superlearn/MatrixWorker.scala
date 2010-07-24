package org.no.ip.bca.superlearn

import java.lang.Math.exp

import org.no.ip.bca.scala.Ranges

class MatrixWorker(
    state: State,
    transposedWeights: Array[Double],
    range: Ranges.Pair,
    dataManager: DataManager) extends java.util.concurrent.Callable[Compute] {
  private type AD = Array[Double]
  
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
    val visibleBias = state.hidden
    
    val start = range.start
    val end = range.end
    val sample = 0.05
    
    val v0 = new AD(w)
    val h0 = new AD(h)
    val v1 = new AD(w)
    val h1 = new AD(h)
    
    val cd = new AD(w * h)
    
    val iter = dataManager.iter(start, end)
    val vAct = new Array[Long](w)
    val hAct = new Array[Long](h)
    var count: Long = 0
    while (iter.hasNext) {
      if (random.nextDouble < sample) {
        if (_canceled) return null
        toBinaryDoubleArray(iter.next, v0)
        mult(v0, weights, hiddenBias)(h0)
        mult(h0, transposedWeights, visibleBias)(v1)
        mult(v1, weights, hiddenBias)(h1)
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
  
  def mult(v: AD, m: AD, bias: AD)(out: AD) = {
    var i = 0
    var y = 0
    var x = 0
    var sum = 0.0
    val vLength = v.length
    val outLength = out.length
    while (i < outLength) {
      sum += m(i) * v(x)
      i += 1
      x += 1
      if (x == vLength) {
        sum = 1.0 / (1.0 + exp(-sum - bias(y)))
        out(y) = if (sum >= random.nextDouble()) 1.0 else 0.0
        y += 1
        x = 0
        sum = 0.0
      }
    }
  }
  
  def explode(v1: AD, h1: AD, v2: AD, h2: AD)(m: AD) {
    var i = 0
    var y = 0
    var x = 0
    var h1y = h1(y)
    var h2y = h2(y)
    val v1Length = v1.length
    val mLength = m.length
    while (i < mLength) {
      m(i) += v1(x) * h1y - v2(x) * h2y
      i += 1
      x += 1
      if (x == v1Length) {
        y += 1
        x = 0
        h1y = h1(y)
        h2y = h2(y)
      }
    }
  }
  
  def mergeActivations(a: AD, b: AD)(out: Array[Long]) = {
    var i = 0
    val outLength = out.length
    while (i < outLength) {
      out(i) += (a(i) - b(i)).toLong
      i += 1
    }
  }
  
  def toBinaryDoubleArray(src: Array[Byte], dest: Array[Double]) {
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
