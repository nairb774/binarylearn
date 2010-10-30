package org.no.ip.bca.superlearn
package math

import org.specs._
import Implicits._

object MatrixSpec extends MatrixTest
class MatrixTest extends SpecificationWithJUnit {
  type ValType = Double
  trait A
  trait B

  "constructor" should {
    "fail with" >> {
      "empty array" >> { new Matrix[ValType, A, B](1, 1, new Array(0)) must throwAn[AssertionError] }
      "zero rows" >> { new Matrix[ValType, A, B](0, 1, new Array(0)) must throwAn[AssertionError] }
      "negative rows" >> { new Matrix[ValType, A, B](-1, 1, new Array(1)) must throwAn[AssertionError] }
      "zero columns" >> { new Matrix[ValType, A, B](1, 0, new Array(0)) must throwAn[AssertionError] }
      "negative columns" >> { new Matrix[ValType, A, B](1, -1, new Array(1)) must throwAn[AssertionError] }
      "mismatched sizes" >> { new Matrix[ValType, A, B](1, 1, new Array(2)) must throwAn[AssertionError] }
    }
    "pass work with matching sizes" >> { new Matrix[ValType, A, B](1, 1, new Array(1)) must notBeNull }
  }
  "empty" should {
    val m = Matrix.withSize[ValType, A, B](5, 9)
    "return a" >> {
      // Check return type explicitly
      val e: Matrix[ValType, A, B] = m.empty
      "new matrix" >> { e must notBe(m) }
      "matrix with a new array" >> { e.a must notBe(m.a) }
      "matrix with the right rows" >> { e.rows must beEqualTo(m.rows) }
      "matrix with the right columns" >> { e.columns must beEqualTo(m.columns) }
    }
  }
  "elementCount" should {
    "return the total number of elements in the array" >> {
      Matrix.withSize[ValType, A, B](5, 9).elementCount must beEqualTo(5 * 9)
    }
  }
  "T [transpose]" should {
    val m = new Matrix[ValType, A, B](2, 3, Array(1, 2, 3, 4, 5, 6))
    "return a" >> {
      // Check return type explicitly
      val t: Matrix[ValType, B, A] = m.T
      "new matrix" >> { t must notBe(m) }
      "matrix with a new array" >> { t.a must notBe(m.a) }
      "matrix with a transposed rows" >> { t.rows must beEqualTo(m.columns) }
      "matrix with a transposed height" >> { t.columns must beEqualTo(m.rows) }
      "matrix with transposed values" >> { t.a must matchArray(Array(1, 3, 5, 2, 4, 6)) }
    }
  }
}