package org.no.ip.bca.superlearn

import javax.management.ObjectName
import java.lang.management.ManagementFactory
import java.io.File
import scala.reflect.BeanProperty
import net.lag.configgy.Configgy
import org.no.ip.bca.scala.Ranges

object ConfigHelper {
  lazy val fullRange = Ranges(0, Configgy.config("data.max").toInt)
  lazy val imageFolder = new File(Configgy.config("image.folder"))
  lazy val matrixFolder = new File(Configgy.config("matrix.folder"))
  lazy val matrixRecorder = new MatrixRecorder()
  lazy val matrixRecorderHost = Configgy.config("recorder.host")
  lazy val matrixRecorderPort = Configgy.config("recorder.port").toInt
  lazy val memMapSource = {
    val dataFile = new File(Configgy.config("data.file"))
    val size = Configgy.config("data.size").toInt
    val metaSize = Configgy.config("data.metasize").toInt
    new MemMapSource(dataFile, size, metaSize)
  }
  lazy val processors = Configgy.config.getInt("processors", Runtime.getRuntime.availableProcessors)
  lazy val serverHost = Configgy.config("server.host")
  lazy val serverPort = Configgy.config("server.port").toInt
  lazy val timeout = Configgy.config("timeout").toInt

  def configState = {
    val momentumMix = Configgy.config("state.momentummix").toDouble
    val sample = Configgy.config("state.sample").toDouble
    val steps = Configgy.config("state.steps").toInt
    val epsilon = Configgy.config("state.epsilon").toDouble
    ConfigState(sample, steps, momentumMix, epsilon)
  }
  def clientConfig = {
    val configState = this.configState
    ClientConfig(configState.sample, configState.steps)
  }
}

case class ConfigState(
  sample: Double,
  steps: Int,
  momentumMix: Double,
  epsilon: Double)

