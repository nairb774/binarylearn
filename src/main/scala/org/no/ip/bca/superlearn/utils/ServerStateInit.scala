package org.no.ip.bca.superlearn
package utils

import org.no.ip.bca.scala.{ FastRandom, Ranges }
import math._

import se.scalablesolutions.akka.actor.Actor.actorOf

object ServerStateInit {
  def main(args: Array[String]): Unit = {
    val recorder = ConfigHelper.matrixRecorder
    val v = args(0).toInt
    val h = args(1).toInt

    val span = 0.000001
    val halfSpan = span / 2.0

    val random = new FastRandom

    val weights: Matrix.Immutable[Side.H, Side.V] = Matrix.immutable.matrix(h, v) | { _ => span * random.nextDouble - halfSpan }
    val visible = Matrix.immutable.vector[Side.V](v)
    val hidden = Matrix.immutable.vector[Side.H](h)

    val momentum = State(Matrix.immutable.matrix(h, v), Matrix.immutable.vector[Side.H](h), Matrix.immutable.vector[Side.V](v))
    val state = State(weights, hidden, visible)
    val compute = Compute(Matrix.immutable.matrix(h, v), Matrix.immutable.vector[Side.V](v), Matrix.immutable.vector[Side.H](h), 0)
    val ss = ServerState(state, momentum, compute, ConfigHelper.configState)
    recorder ! MatrixRecorder.Record(System.currentTimeMillis, ss)
  }
}