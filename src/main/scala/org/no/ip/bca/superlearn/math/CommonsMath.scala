package org.no.ip.bca.superlearn.math

import scala.math._
import org.apache.commons.math.linear.BlockRealMatrix
import org.apache.commons.math.linear.RealMatrixChangingVisitor
import org.apache.commons.math.linear.RealMatrixPreservingVisitor
import org.apache.commons.math.stat.StatUtils

object CommonsMatrix extends MatrixFactory[CommonsMatrix] {
  def matrix[L, R](rows: Int, columns: Int): CommonsMatrix[L, R] =
    new CommonsMatrix(new BlockRealMatrix(rows, columns))
  def from[L, R](array: Array[Array[Double]]): CommonsMatrix[L, R] =
    new CommonsMatrix(new BlockRealMatrix(array))
}

final class CommonsMatrix[L, R](private[CommonsMatrix] val m: BlockRealMatrix) extends MatrixImmutable[L, R] {
  type Self[A, B] = CommonsMatrix[A, B]
  def this(a: Array[Array[Double]]) = this(new BlockRealMatrix(a))

  private def notImplemented = throw new UnsupportedOperationException

  def columns: Int = m.getColumnDimension
  def rows: Int = m.getRowDimension

  def +(d: Double) = new Self(m.scalarAdd(d))
  def +(d: Double, out: Self[L, R]) = notImplemented
  def +=(d: Double) = notImplemented

  def +(other: Self[L, R]) = new Self(m.add(other.m))
  def +(other: Self[L, R], out: Self[L, R]) = notImplemented
  def +=(other: Self[L, R]) = notImplemented

  def -(other: Self[L, R]) = new Self(m.subtract(other.m))
  def -(other: Self[L, R], out: Self[L, R]) = notImplemented
  def -=(other: Self[L, R]) = notImplemented

  def *(d: Double) = new Self(m.scalarMultiply(d).asInstanceOf[BlockRealMatrix])
  def *(d: Double, out: Self[L, R]) = notImplemented
  def *=(d: Double) = notImplemented

  def *[O](other: Self[R, O]) = new Self(m.multiply(other.m))
  def *[O](other: Self[R, O], out: Self[L, O]) = notImplemented

  def |[A, B](f: (Int, Int, Double) => Double) = {
    val o = m.copy
    o.walkInOptimizedOrder(new RealMatrixChangingVisitor {
      def start(rows: Int, columns: Int, startRow: Int, endRow: Int, startColumn: Int, endColumn: Int) = {}
      def visit(row: Int, column: Int, value: Double) = f(column, row, value)
      def end = 0.0
    })
    new Self(o)
  }
  def |[A, B](f: (Int, Int, Double) => Double, out: Self[A, B]) = notImplemented

  override def foreach(f: (Int, Int, Double) => Unit) = {
    m.walkInOptimizedOrder(new RealMatrixPreservingVisitor {
      def start(rows: Int, columns: Int, startRow: Int, endRow: Int, startColumn: Int, endColumn: Int) = {}
      def visit(row: Int, column: Int, value: Double) = f(column, row, value)
      def end = 0.0
    })
  }
  def T = new Self(m.transpose)
  def reduce(f: (Double, Double) => Double): Double = {
    val data = m.getData()
    (data(0).reduceLeft(f) /: data.drop(1)) { (a, b) => f(a, b.reduceLeft(f)) }
  }
  def angle(other: Self[L, R]): Double = {
    val otherm = other.m
    val sums = ((0.0, 0.0, 0.0) /: (0 until rows)) { (sum, i) =>
      val mRow = m.getRowVector(i)
      val otherRow = otherm.getRowVector(i)
      (sum._1 + mRow.dotProduct(otherRow), sum._2 + mRow.dotProduct(mRow), sum._3 + otherRow.dotProduct(otherRow))
    }
    acos(sums._1 / (sqrt(sums._2) * sqrt(sums._3)))
  }
  
  def toMutable = Matrix.mutable.from(m.getData)
  def asArray = {
    var row = 0
    val columns = this.columns
    val out = new Array[Double](rows * columns)
    while (row < rows) {
      System.arraycopy(m.getRow(row), 0, out, row * columns, columns)
      row += 1
    }
    out
  }
  def zero = new Self(new BlockRealMatrix(rows, columns))
}