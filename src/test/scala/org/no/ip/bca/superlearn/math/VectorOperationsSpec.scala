package org.no.ip.bca.superlearn
package math

import org.specs._

import Implicits._

object VectorOperationsSpec extends VectorOperationsTest
class VectorOperationsTest extends SpecificationWithJUnit {
  type ValType = Double
  trait A
  trait B
  "*[O]{b: Matrix[PrimType, T, O]}: Vector[PrimType, O] => Vector[PrimType, O]" should {
    val a = new Vector[ValType, A](Array(2, 3))
    val b = new Matrix[ValType, A, B](2, 2, Array(2, 4, 3, 5))
    val c = new Vector[ValType, B](new Array(2))
    val ans = new Vector[ValType, B](Array(16, 21))
    "produce correct results" >> {
      (a * b)(c) must matchVector(ans)
    }
  }
  "^" should {
    val a = new Vector[ValType, A](Array(2, 3))
    val b = new Vector[ValType, B](Array(4, 5, 6))
    val c = Matrix.withSize[ValType, A, B](2, 3)
    val ans = new Matrix[ValType, A, B](2, 3, Array(8, 12, 10, 15, 12, 18))
    "" >> {
      (a ^ b)(c) must matchMatrix(ans)
    }
  }
}