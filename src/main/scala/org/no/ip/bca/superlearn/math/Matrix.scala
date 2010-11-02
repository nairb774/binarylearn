package org.no.ip.bca.superlearn.math

import java.util.Arrays
import org.apache.commons.math.linear.ArrayRealVector
import org.apache.commons.math.linear.BlockRealMatrix
import org.apache.commons.math.linear.RealMatrixChangingVisitor
import org.apache.commons.math.linear.RealMatrixPreservingVisitor

object Matrix {
  def withSize[L, R](r: Int, c: Int): Matrix[L, R] =
    new Matrix(new BlockRealMatrix(r, c))
}

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
  override def toString = Arrays.toString(a)
}

object RVector {
  def withLength[T](l: Int): RVector[T] = new RVector(new Array(l))
}

final class RVector[T](a: Array[Double]) extends Vector[T](a) {
  type Me[A] = RVector[A]
  def mkNew[O](v: Array[Double]): Me[O] = new RVector(v)
  def *[T, O](m: Matrix[T, O]): Me[O] = mkNew(m.m.preMultiply(a))
  def T: CVector[T] = new CVector(a.clone)
}

final class CVector[T](a: Array[Double]) extends Vector[T](a) {
  type Me[A] = CVector[A]
  def mkNew[O](v: Array[Double]): Me[O] = new CVector(v)
  def *[O, T](m: Matrix[O, T]): Me[O] = mkNew(m.m.operate(a))
  def T: RVector[T] = new RVector(a.clone)
  def ^[O](c: RVector[O]): Matrix[T, O] = {
    val am = new BlockRealMatrix(length, 1)
    am.setColumn(0, a)
    val bm = new BlockRealMatrix(1, c.length)
    bm.setRow(0, c.a)
    new Matrix(am.multiply(bm))
  }
}

@serializable
final class Matrix[L, R](val m: BlockRealMatrix) {
  def this(a: Array[Array[Double]]) = this(new BlockRealMatrix(a))
  def columns: Int = m.getColumnDimension
  def rows: Int = m.getRowDimension
  def +(d: Double): Matrix[L, R] = new Matrix(m.scalarAdd(d))
  def +(other: Matrix[L, R]): Matrix[L, R] = new Matrix(m.add(other.m))
  def -(d: Double): Matrix[L, R] = this + (-d)
  def -(other: Matrix[L, R]): Matrix[L, R] = new Matrix(m.subtract(other.m))
  def *(d: Double): Matrix[L, R] = new Matrix(m.scalarMultiply(d).asInstanceOf[BlockRealMatrix])
  def *[O](other: Matrix[R, O]): Matrix[L, O] = new Matrix(m.multiply(other.m))
  def /(d: Double): Matrix[L, R] = this * (1 / d)
  def |[A, B](f: Double => Double): Matrix[A, B] = this | { (_, _, v) => f(v) }
  def |[A, B](f: (Int, Int, Double) => Double): Matrix[A, B] = {
    val o = m.copy
    o.walkInOptimizedOrder(new RealMatrixChangingVisitor {
      def start(rows: Int, columns: Int, startRow: Int, endRow: Int, startColumn: Int, endColumn: Int) = {}
      def visit(row: Int, column: Int, value: Double) = f(column, row, value)
      def end = 0.0
    })
    new Matrix(o)
  }
  def foreach(f: (Int, Int, Double) => Unit): Unit = {
    m.walkInOptimizedOrder(new RealMatrixPreservingVisitor {
      def start(rows: Int, columns: Int, startRow: Int, endRow: Int, startColumn: Int, endColumn: Int) = {}
      def visit(row: Int, column: Int, value: Double) = f(column, row, value)
      def end = 0.0
    })
  }
  def T: Matrix[R, L] = new Matrix(m.transpose)
  def reduce(f: (Double, Double) => Double): Double = {
    val data = m.getData()
    (data(0).reduceLeft(f) /: data.drop(1)) { (a, b) => f(a, b.reduceLeft(f)) }
  }
  def angle(other: Matrix[L, R]): Double = {
    val otherm = other.m
    val sums = ((0.0, 0.0, 0.0) /: (0 until rows)) { (sum, i) =>
      val mRow = m.getRowVector(i)
      val otherRow = otherm.getRowVector(i)
      (sum._1 + mRow.dotProduct(otherRow), sum._2 + mRow.dotProduct(mRow), sum._3 + otherRow.dotProduct(otherRow))
    }
    Math.acos(sums._1 / (Math.sqrt(sums._2) * Math.sqrt(sums._3)))
  }
}