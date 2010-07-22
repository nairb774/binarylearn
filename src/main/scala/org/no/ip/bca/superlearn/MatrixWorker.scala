package org.no.ip.bca.superlearn

import org.no.ip.bca.scala.Ranges
import UtilMethods._

class MatrixWorker(
    matrix: Matrix,
    transpose: Matrix,
    range: Ranges.Pair,
    dataManager: DataManager) extends java.util.concurrent.Callable[(Matrix, Long)] {
  @volatile
  private var _canceled = false
  def cancel = _canceled = true
  
  def call: (Matrix, Long) = {
    val matrix = this.matrix.m
    val w = this.matrix.w
    val h = this.matrix.h
    val transpose = this.transpose.m
    
    val start = range.start
    val end = range.end
    val random = new FastRandom
    val sample = 0.05
    
    val v1 = new Array[Double](w)
    val h1 = new Array[Double](h)
    val v2 = new Array[Double](w)
    val h2 = new Array[Double](h)
    
    val m = new Array[Double](w * h)
    
    val iter = dataManager.iter(start, end)
    var count: Long = 0
    while (iter.hasNext) {
      if (random.nextDouble < sample) {
        if (_canceled) return null
        toBinaryDoubleArray(iter.next, v1)
        mult(v1, matrix, w, h, random, h1)
        mult(h1, transpose, h, w, random, v2)
        mult(v2, matrix, w, h, random, h2)
        explode(v1, h1, v2, h2, m)
        count += 1
      } else {
        iter.skip
      }
    }
    (Matrix(w, h, m), count)
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
