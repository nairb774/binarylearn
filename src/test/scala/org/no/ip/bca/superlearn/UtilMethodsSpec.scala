package org.no.ip.bca.superlearn

import org.specs._

object UtilMethodsSpec extends UtilMethodsTest
class UtilMethodsTest extends SpecificationWithJUnit {
  "transpose" should {
    def handle(in: Array[Double])(w: Int, h: Int)(expect: Array[Double]) = {
      val out = new Array[Double](in.length)
      UtilMethods.transpose(in, w, h, out)
      out must matchArray(expect)
    }
    "simple 1x1" >> handle(Array(1.0))(1, 1)(Array(1.0))
    "simple 2x2" >> handle(Array(1.0, 2.0, 3.0, 4.0))(2, 2)(Array(1.0, 3.0, 2.0, 4.0))
    "simple 1x2" >> handle(Array(1.0, 2.0))(1, 2)(Array(1.0, 2.0))
    "simple 2x1" >> handle(Array(1.0, 2.0))(2, 1)(Array(1.0, 2.0))
    "simple 2x3" >> handle(
            Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))(2, 3)(
            Array(1.0, 3.0, 5.0, 2.0, 4.0, 6.0))
    "simple 3x2" >> handle(
            Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))(3, 2)(
            Array(1.0, 4.0, 2.0, 5.0, 3.0, 6.0))
  }
  
  "toBinaryDoubleArray" should {
    "simple" >> {
      val in = Array[Byte](0, 1, 2, 0)
      val out = new Array[Double](in.length)
      UtilMethods.toBinaryDoubleArray(in, out);
      out must matchArray(Array(0.0, 1.0, 1.0, 0.0))
    }
  }
  
  case class matchArray(a: Array[Double]) extends matcher.Matcher[Array[Double]] {
    import java.util.Arrays
    def apply(v: => Array[Double]) = (Arrays.equals(v, a), "okMessage", "koMessage")
  }
}