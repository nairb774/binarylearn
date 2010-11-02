package org.no.ip.bca.superlearn.math

import org.apache.commons.math.linear.ArrayRealVector
import org.specs._

object MatrixSpecs extends MatrixTest
class MatrixTest extends SpecificationWithJUnit {
  "matrix angle" should {
    "match vector angle" >> {
      val m1 = new Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      val m2 = new Matrix(Array(Array(5.0, 6.0), Array(7.0, 8.0)))
      val a1 = new ArrayRealVector(Array(1.0, 2.0, 3.0, 4.0))
      val a2 = new ArrayRealVector(Array(5.0, 6.0, 7.0, 8.0))
      (m1 angle m2) must beEqualTo(Math.acos((a1 dotProduct a2) / (a1.getNorm * a2.getNorm)))
    }
  }
}