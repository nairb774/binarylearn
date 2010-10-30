package org.no.ip.bca.superlearn
package utils

import net.lag.configgy.Configgy

import org.no.ip.bca.scala.{ FastRandom, Ranges }

import math.{ Matrix, Vector }
import math.Implicits._

object ServerStateInit {
  def main(args: Array[String]): Unit = {
    val w = args(0).toInt
    val h = args(1).toInt
    Configgy configure args(2)

    val span = 8.0 * Math.sqrt(6.0 / (w + h))
    val halfSpan = span / 2.0

    val random = new FastRandom

    val weights = Matrix.withSize[Double, Side.V, Side.H](w, h)
    val visible = Vector.withLength[Double, Side.V](w)
    val hidden = Vector.withLength[Double, Side.H](h)
    val f = { _: Double => span * random.nextDouble - halfSpan }
    weights |< f

    val state = State(weights, hidden, visible)
    val compute = Compute(Matrix withSize (w, h), Vector.withLength[Double, Side.V](w), Vector.withLength[Double, Side.H](h), 0)
    val ss = ServerState(state, compute, ConfigHelper.configState)
    ConfigHelper.matrixRecorder.record(ss)
  }
}