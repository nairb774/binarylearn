package org.no.ip.bca.superlearn.math

import java.util.Arrays
import scala.math._
import org.apache.commons.math.linear.ArrayRealVector
import org.apache.commons.math.linear.BlockRealMatrix
import org.apache.commons.math.stat.StatUtils

/*
@serializable
abstract class Vector[T](val a: Array[Double]) {
  type Me[A] <: Vector[A]
  def length: Int = a.length
  protected def mkNew[A](v: Array[Double]): Me[A]
  def empty: Me[T] = mkNew[T](new Array(length))
  def +(d: Double): Me[T] = this | { n => n + d }
  def +(v: Me[T]): Me[T] = this | { (i, n) => n + v.a(i) }
  def -(d: Double): Me[T] = this | { n => n - d }
  def -(v: Me[T]): Me[T] = this | { (i, n) => n - v.a(i) }
  def *(d: Double): Me[T] = this | { n => n * d }
  def /(d: Double): Me[T] = this | { n => n / d }
  def |[O](f: (Int, Double) => Double): Me[O] = {
    val a_ = a
    val b = new Array[Double](length)
    var i = 0
    while (i < a_.length) {
      b(i) = f(i, a(i))
      i += 1
    }
    mkNew[O](b)
  }
  def |[O](f: Double => Double): Me[O] = this | { (_, v) => f(v) }
  def mean = StatUtils mean a
  def variance = StatUtils variance a
  def variance(mean: Double) = StatUtils.variance(a, mean)
  def array = a.clone
  override def toString = Arrays.toString(a)
}

object RVector {
  def withLength[T](l: Int): RVector[T] = new RVector(new Array(l))
}

final class RVector[T](a: Array[Double]) extends Vector[T](a) {
  type Me[A] = RVector[A]
  def mkNew[O](v: Array[Double]): Me[O] = new RVector(v)
  def *[T, O](m: CommonsMatrix[T, O]): Me[O] = mkNew(m.preMultiply(a))
  def T: CVector[T] = new CVector(a.clone)
}

final class CVector[T](a: Array[Double]) extends Vector[T](a) {
  type Me[A] = CVector[A]
  def mkNew[O](v: Array[Double]): Me[O] = new CVector(v)
  def *[O, T](m: CommonsMatrix[O, T]): Me[O] = mkNew(m.operate(a))
  def T: RVector[T] = new RVector(a.clone)
  def ^[O](c: RVector[O]): CommonsMatrix[T, O] = {
    val am = new BlockRealMatrix(length, 1)
    am.setColumn(0, a)
    val bm = new BlockRealMatrix(1, c.length)
    bm.setRow(0, c.a)
    new CommonsMatrix(am.multiply(bm))
  }
}
*/

trait MatrixFactory[Type[A, B] <: MatrixBase[A, B]] {
  def vector[T](len: Int): Type[T, Unit] = matrix(len, 1)
  def matrix[L, R](rows: Int, columns: Int): Type[L, R]
  def from[T](array: Array[Double]): Type[T, Unit] = from(Array(array))
  def from[L, R](array: Array[Array[Double]]): Type[L, R]
}

object Matrix {
  type Immutable[L, R] = CommonsMatrix[L, R]
  type ImmutableV[T] = Immutable[T, Unit]
  type Mutable[L, R] = JBlasMatrix[L, R]
  type MutableV[T] = Mutable[T, Unit]
  val immutable: MatrixFactory[Immutable] = CommonsMatrix
  val mutable: MatrixFactory[Mutable] = JBlasMatrix
}

@serializable
trait MatrixBase[L, R] { self =>
  type Self[A, B] <: MatrixBase[A, B]
  def columns: Int
  def rows: Int
  def size = rows * columns

  def +(d: Double): Self[L, R]
  def +(d: Double, out: Self[L, R]): Self[L, R]
  def +(other: Self[L, R]): Self[L, R]
  def +(other: Self[L, R], out: Self[L, R]): Self[L, R]

  def -(d: Double): Self[L, R] = self + (-d)
  def -(d: Double, out: Self[L, R]): Self[L, R] = self + (-d, out)
  def -(other: Self[L, R]): Self[L, R]
  def -(other: Self[L, R], out: Self[L, R]): Self[L, R]

  def *(d: Double): Self[L, R]
  def *(d: Double, out: Self[L, R]): Self[L, R]

  def *[O](other: Self[R, O]): Self[L, O]
  def *[O](other: Self[R, O], out: Self[L, O]): Self[L, O]

  def /(d: Double): Self[L, R] = self * (1 / d)
  def /(d: Double, out: Self[L, R]): Self[L, R] = self * (1 / d, out)

  def T: Self[R, L]

  def |[A, B](f: Double => Double): Self[A, B] = self | { (_, _, v) => f(v) }
  def |[A, B](f: Double => Double, out: Self[A, B]): Self[A, B] = self | ({ (_, _, v) => f(v) }, out)

  /** (column, row, value) */
  def |[A, B](f: (Int, Int, Double) => Double): Self[A, B]
  def |[A, B](f: (Int, Int, Double) => Double, out: Self[A, B]): Self[A, B]

  def foreach(f: (Int, Int, Double) => Unit): Unit = self | ({ (c, r, v) => f(c, r, v); v }, self.asInstanceOf[Self[L, R]])

  def zero: Self[L, R]
  /** Row0, Row1, ... */
  def asArray: Array[Double]
  def mean = StatUtils mean asArray
  def variance(mean: Double) = StatUtils variance (asArray, mean)
}

trait MatrixImmutable[L, R] extends MatrixBase[L, R] { self =>
  type Self[A, B] <: MatrixImmutable[A, B]

  def toMutable: Matrix.Mutable[L, R]
}

trait MatrixMutable[L, R] extends MatrixBase[L, R] { self =>
  type Self[A, B] <: MatrixMutable[A, B]

  def +=(d: Double): Self[L, R] = {
    self.+(d, self.asInstanceOf[Self[L, R]])
    self.asInstanceOf[Self[L, R]]
  }

  def +=(other: Self[L, R]): Self[L, R] = {
    self.+(other, self.asInstanceOf[Self[L, R]])
    self.asInstanceOf[Self[L, R]]
  }

  def -=(d: Double): Self[L, R] = {
    self += -d
    self.asInstanceOf[Self[L, R]]
  }

  def -=(other: Self[L, R]): Self[L, R] = {
    self.-(other, self.asInstanceOf[Self[L, R]])
    self.asInstanceOf[Self[L, R]]
  }

  def *=(d: Double): Self[L, R] = {
    self.*(d, self.asInstanceOf[Self[L, R]])
    self.asInstanceOf[Self[L, R]]
  }

  def /=(d: Double): Self[L, R] = self *= (1 / d)

  def |=[A, B](f: Double => Double): Self[L, R] = self |= { (_, _, v) => f(v) }

  /** (column, row, value) */
  def |=(f: (Int, Int, Double) => Double): Self[L, R] = {
    self.|(f, self.asInstanceOf[Self[L, R]])
    self.asInstanceOf[Self[L, R]]
  }

  def `zero=`: Self[L, R]
  
  def toImmutable: Matrix.Immutable[L, R]
}
