package org.no.ip.bca.superlearn

import org.no.ip.bca.scala.Ranges
import UtilMethods._

class MatrixWorker(
    state: State,
    transposedWeights: Array[Double],
    range: Ranges.Pair,
    dataManager: DataManager) extends java.util.concurrent.Callable[Compute] {
  @volatile
  private var _canceled = false
  def cancel = _canceled = true
  
  def call: Compute = {
    val weights = state.weights
    val w = state.w
    val h = state.h
    val transposedWeights = this.transposedWeights
    val hiddenBias = state.hidden
    val visibleBias = state.hidden
    
    val start = range.start
    val end = range.end
    val random = new FastRandom
    val sample = 0.05
    
    val v0 = new Array[Double](w)
    val h0 = new Array[Double](h)
    val v1 = new Array[Double](w)
    val h1 = new Array[Double](h)
    
    val cd = new Array[Double](w * h)
    
    val iter = dataManager.iter(start, end)
    val v0act = new Array[Long](w)
    val h0act = new Array[Long](h)
    val v1act = new Array[Long](w)
    val h1act = new Array[Long](h)
    var count: Long = 0
    while (iter.hasNext) {
      if (random.nextDouble < sample) {
        if (_canceled) return null
        toBinaryDoubleArray(iter.next, v0)
        mult(v0, weights, w, h, hiddenBias, random, h0)
        mult(h0, transposedWeights, h, w, visibleBias, random, v1)
        mult(v1, weights, w, h, hiddenBias, random, h1)
        explode(v0, h0, v1, h1, cd)
        sum(v0, v0act, v0act)
        sum(h0, h0act, h0act)
        sum(v1, v1act, v1act)
        sum(h1, h1act, h1act)
        count += 1
      } else {
        iter.skip
      }
    }
    Compute(cd, v0act, h0act, v1act, h0act, count)
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
