package org.no.ip.bca.superlearn
package utils

import net.lag.configgy.Configgy

import org.no.ip.bca.scala.{ FastRandom, Ranges }

import math._

object ServerStateInit {
  def main(args: Array[String]): Unit = {
    val w = args(0).toInt
    val h = args(1).toInt
    Configgy configure args(2)

    val span = 0.000001
    val halfSpan = span / 2.0

    val random = new FastRandom

    val weights: Matrix[Side.V, Side.H] = Matrix.withSize(w, h) | { _ => span * random.nextDouble - halfSpan }
    val visible = RVector.withLength[Side.V](w)
    val hidden = RVector.withLength[Side.H](h)

    val momentum = State(Matrix withSize (w, h), RVector.withLength[Side.H](h), RVector.withLength[Side.V](w))
    val state = State(weights, hidden, visible)
    val compute = Compute(Matrix withSize (w, h), RVector.withLength[Side.V](w), RVector.withLength[Side.H](h), 0)
    val ss = ServerState(state, momentum, compute, ConfigHelper.configState)
    MatrixRecorder.handleState(System.currentTimeMillis, ss)
  }
}