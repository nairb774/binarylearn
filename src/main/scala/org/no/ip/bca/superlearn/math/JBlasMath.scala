package org.no.ip.bca.superlearn.math

import org.jblas.DoubleMatrix

object JBlasMatrix extends MatrixFactory[JBlasMatrix] {
  def matrix[L, R](rows: Int, columns: Int): JBlasMatrix[L, R] =
    new JBlasMatrix(DoubleMatrix.zeros(rows, columns))
  def from[L, R](array: Array[Array[Double]]): JBlasMatrix[L, R] =
    new JBlasMatrix(new DoubleMatrix(array))
  def ger[L, R](alpha: Double, x: JBlasMatrix[L, Unit], y: JBlasMatrix[R, Unit], out: JBlasMatrix[L, R]) = {
    org.jblas.SimpleBlas.ger(alpha, x.m, y.m, out.m)
  }
}

class JBlasMatrix[L, R](private[JBlasMatrix] val m: DoubleMatrix) extends MatrixMutable[L, R] {
  type Self[A, B] = JBlasMatrix[A, B]
  def columns = m.columns
  def rows = m.rows

  def +(d: Double) = new Self(m.add(d))
  def +(d: Double, out: Self[L, R]) = {
    m.addi(d, out.m)
    out
  }

  def +(other: Self[L, R]) = new Self(m.add(other.m))
  def +(other: Self[L, R], out: Self[L, R]) = {
    m.addi(other.m, out.m)
    out
  }

  def -(other: Self[L, R]) = new Self(m.sub(other.m))
  def -(other: Self[L, R], out: Self[L, R]) = {
    m.subi(other.m, out.m)
    out
  }

  def *(d: Double) = new Self(m.mul(d))
  def *(d: Double, out: Self[L, R]) = {
    m.muli(d, out.m)
    out
  }

  def *[O](other: Self[R, O]) = new Self(m.mmul(other.m))
  def *[O](other: Self[R, O], out: Self[L, O]) = {
    m.mmuli(other.m, out.m)
    out
  }

  def T = new Self(m.transpose)

  def |[A, B](f: (Int, Int, Double) => Double) = this.|(f, new Self[A, B](DoubleMatrix.zeros(rows, columns)))
  def |[A, B](f: (Int, Int, Double) => Double, out: Self[A, B]) = {
    var row = 0
    var column = 0
    var i = 0
    val rows = this.rows
    val columns = this.columns
    val data = m.data
    val dataLength = data.length
    val outData = out.m.data
    while (i < dataLength) {
      outData(i) = f(column, row, data(i))
      i += 1
      column += 1
      if (column == columns) {
        column = 0
        row += 1
      }
    }
    out
  }
  
  def toImmutable = Matrix.immutable.from(m.toArray2)
  def zero = new Self(new DoubleMatrix(rows, columns))
  def `zero=` = {
    java.util.Arrays.fill(m.data, 0.0)
    this
  }
  def asArray = m.data
}