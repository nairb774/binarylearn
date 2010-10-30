package org.no.ip.bca.superlearn.math

import Types._

object Types {
  type MatrixB[L, R] = Matrix[Byte, L, R]
  type MatrixD[L, R] = Matrix[Double, L, R]
  type MatrixL[L, R] = Matrix[Long, L, R]
  type VectorB[T] = Vector[Byte, T]
  type VectorD[T] = Vector[Double, T]
  type VectorL[T] = Vector[Long, T]
}

object Implicits {
  import Types._

  implicit def toOpsMatrixD[L, R](a: MatrixD[L, R]): MatrixDOps[L, R] = new MatrixDOps(a)
  implicit def MatrixDtoS[L, R](a: MatrixD[L, R]): S[MatrixD[L, R]] = new S(a)
  implicit def toOpsVectorD[T](a: VectorD[T]): VectorDOps[T] = new VectorDOps(a)
  implicit def VectorDtoS[T](a: VectorD[T]): S[VectorD[T]] = new S(a)

  class S[I](c: I) {
    def <=[O](f: I => O) = f(c)
  }
}
