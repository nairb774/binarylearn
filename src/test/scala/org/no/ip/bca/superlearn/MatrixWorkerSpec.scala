package org.no.ip.bca.superlearn

import org.specs._

object MatrixWorkerSpec extends MatrixWorkerTest
class MatrixWorkerTest extends SpecificationWithJUnit {
  type AL = Array[Long]

  "explode" should {
    val mw = new MatrixWorker(null, null, null, null, null, 0.0, null)
    "everything matches" >> {
      val v0 = Array[Byte](0, 1)
      val h0 = Array[Byte](1, 1)
      val v1 = Array[Byte](0, 1)
      val h1 = Array[Byte](1, 1)
      val out = new AL(4)
      mw.explode(v0, h0, v1, h1)(out)
      out must matchLongArray(Array(0, 0, 0, 0))
    }

    "1" >> {
      val v0 = Array[Byte](0, 1)
      val h0 = Array[Byte](1, 1)
      val v1 = Array[Byte](1, 1)
      val h1 = Array[Byte](0, 1)
      val out = new AL(4)
      mw.explode(v0, h0, v1, h1)(out)
      out must matchLongArray(Array(0, 1, -1, 0))
    }

    "2" >> {
      val v0 = Array[Byte](1, 1)
      val h0 = Array[Byte](0, 1)
      val v1 = Array[Byte](0, 1)
      val h1 = Array[Byte](1, 0)
      val out = new AL(4)
      mw.explode(v0, h0, v1, h1)(out)
      out must matchLongArray(Array(0, -1, 1, 1))
    }
  }

  "mergeActivations" should {
    val mw = new MatrixWorker(null, null, null, null, null, 0.0, null)
    "1" >> {
      val v0 = Array[Byte](0, 1)
      val v1 = Array[Byte](1, 1)
      val out = new AL(2)
      mw.mergeActivations(v0, v1)(out)
      out must matchLongArray(Array(-1, 0))
    }
  }
}