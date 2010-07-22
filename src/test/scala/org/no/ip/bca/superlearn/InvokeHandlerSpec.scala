package org.no.ip.bca.superlearn

import java.io._
import java.util.concurrent._

import scala.actors.Actor._
import org.specs._


object InvokeHandlerSpec extends InvokeHandlerTest
class InvokeHandlerTest extends SpecificationWithJUnit {"a" should {
  "b" >> {
    val block = new CountDownLatch(1)
    val cis = new PipedInputStream
    val sos = new PipedOutputStream(cis)
    val ioa = new InvokeOut(new DataOutputStream(sos))
    ioa.start
    val runnable = new Runnable {
      def run = block.countDown
    }
    val invokeIn = new InvokeIn(new DataInputStream(cis))
    invokeIn + (0 -> runnable)
    invokeIn.start
    val handler = ioa.open[Runnable](0)
    handler.run
    handler.run
    handler.run
    try {
      block.await(100, TimeUnit.MILLISECONDS) mustBe true
    } finally {
      ioa.stop
    }
  }
}}