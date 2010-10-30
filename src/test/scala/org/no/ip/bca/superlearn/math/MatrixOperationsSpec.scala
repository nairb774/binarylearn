package org.no.ip.bca.superlearn
package math

import org.specs._

import Implicits._

object MatrixOperationsSpec extends MatrixOperationsTest
class MatrixOperationsTest extends SpecificationWithJUnit {
  type ValType = Double
  trait A
  trait B
  "+{b: MyType}: MyType => MyType" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val c = a.empty
    val ans = new Matrix[ValType, A, B](2, 2, Array(6, 8, 10, 12))

    "return a function" >> { (a + b) must haveSuperClass[Matrix[ValType, A, B] => Matrix[ValType, A, B]] }
    "return a correct result when completed" >> { (a + b)(c) must matchMatrix(ans) }
    "fail when" >> {
      "operand widths don't match" >> { (a + Matrix.withSize(1, 2))(c) must throwA[AssertionError] }
      "operand heights don't match" >> { (a + Matrix.withSize(2, 1))(c) must throwA[AssertionError] }
      "completed with an incorrect width" >> { (a + b)(Matrix.withSize(1, 2)) must throwA[AssertionError] }
      "completed with an incorrect height" >> { (a + b)(Matrix.withSize(2, 1)) must throwA[AssertionError] }
    }
  }
  "+<" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val ans = new Matrix[ValType, A, B](2, 2, Array(6, 8, 10, 12))
    "return correct result when completed" >> { (a +< b) must matchMatrix(ans) }
    "left operand contains correct result when completed" >> { (a +< b); a must matchMatrix(ans) }
    "returned value should be left operand" >> { (a +< b) must be(a) }
    "fail when" >> {
      "operand widths don't match" >> { (a +< Matrix.withSize(1, 2)) must throwA[AssertionError] }
      "operand heights don't match" >> { (a +< Matrix.withSize(2, 1)) must throwA[AssertionError] }
    }
  }
  "+>" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val ans = new Matrix[ValType, A, B](2, 2, Array(6, 8, 10, 12))
    "return correct result when completed" >> { (a +> b) must matchMatrix(ans) }
    "right operand contains correct result when completed" >> { (a +> b); b must matchMatrix(ans) }
    "returned value should be left operand" >> { (a +> b) must be(b) }
    "fail when" >> {
      "operand widths don't match" >> { (a +> Matrix.withSize(1, 2)) must throwA[AssertionError] }
      "operand heights don't match" >> { (a +> Matrix.withSize(2, 1)) must throwA[AssertionError] }
    }
  }
  "+{b: MyType, c: MyType}: MyType" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val c = a.empty
    val ans = new Matrix[ValType, A, B](2, 2, Array(6, 8, 10, 12))

    "return a correct result when completed" >> { (a + (b, c)) must matchMatrix(ans) }
    "fail when" >> {
      "operand widths don't match" >> { (a + (Matrix.withSize(1, 2), c)) must throwA[AssertionError] }
      "operand heights don't match" >> { (a + (Matrix.withSize(2, 1), c)) must throwA[AssertionError] }
      "completed with an incorrect width" >> { (a + (b, Matrix.withSize(1, 2))) must throwA[AssertionError] }
      "completed with an incorrect height" >> { (a + (b, Matrix.withSize(2, 1))) must throwA[AssertionError] }
    }
  }
  
  "-{b: MyType}: MyType => MyType" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val c = a.empty
    val ans = new Matrix[ValType, A, B](2, 2, Array(-4, -4, -4, -4))

    "return a function" >> { (a - b) must haveSuperClass[Matrix[ValType, A, B] => Matrix[ValType, A, B]] }
    "return a correct result when completed" >> { (a - b)(c) must matchMatrix(ans) }
    "fail when" >> {
      "operand widths don't match" >> { (a - Matrix.withSize(1, 2))(c) must throwA[AssertionError] }
      "operand heights don't match" >> { (a - Matrix.withSize(2, 1))(c) must throwA[AssertionError] }
      "completed with an incorrect width" >> { (a - b)(Matrix.withSize(1, 2)) must throwA[AssertionError] }
      "completed with an incorrect height" >> { (a - b)(Matrix.withSize(2, 1)) must throwA[AssertionError] }
    }
  }
  "-<" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val ans = new Matrix[ValType, A, B](2, 2, Array(-4, -4, -4, -4))
    "return correct result when completed" >> { (a -< b) must matchMatrix(ans) }
    "left operand contains correct result when completed" >> { (a -< b); a must matchMatrix(ans) }
    "returned value should be left operand" >> { (a -< b) must be(a) }
    "fail when" >> {
      "operand widths don't match" >> { (a -< Matrix.withSize(1, 2)) must throwA[AssertionError] }
      "operand heights don't match" >> { (a -< Matrix.withSize(2, 1)) must throwA[AssertionError] }
    }
  }
  "->" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val ans = new Matrix[ValType, A, B](2, 2, Array(-4, -4, -4, -4))
    "return correct result when completed" >> { (a -> b) must matchMatrix(ans) }
    "right operand contains correct result when completed" >> { (a -> b); b must matchMatrix(ans) }
    "returned value should be left operand" >> { (a -> b) must be(b) }
    "fail when" >> {
      "operand widths don't match" >> { (a -> Matrix.withSize(1, 2)) must throwA[AssertionError] }
      "operand heights don't match" >> { (a -> Matrix.withSize(2, 1)) must throwA[AssertionError] }
    }
  }
  "-{b: MyType, c: MyType}: MyType" should {
    val a = new Matrix[ValType, A, B](2, 2, Array(1, 2, 3, 4))
    val b = new Matrix[ValType, A, B](2, 2, Array(5, 6, 7, 8))
    val c = a.empty
    val ans = new Matrix[ValType, A, B](2, 2, Array(-4, -4, -4, -4))

    "return a correct result when completed" >> { (a - (b, c)) must matchMatrix(ans) }
    "fail when" >> {
      "operand widths don't match" >> { (a - (Matrix.withSize(1, 2), c)) must throwA[AssertionError] }
      "operand heights don't match" >> { (a - (Matrix.withSize(2, 1), c)) must throwA[AssertionError] }
      "completed with an incorrect width" >> { (a - (b, Matrix.withSize(1, 2))) must throwA[AssertionError] }
      "completed with an incorrect height" >> { (a - (b, Matrix.withSize(2, 1))) must throwA[AssertionError] }
    }
  }
}