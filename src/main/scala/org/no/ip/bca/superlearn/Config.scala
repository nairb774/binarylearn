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
  lazy val matrixRecorder = new MatrixRecorder(matrixFolder)
  lazy val memMapSource = {
    val dataFile = new File(Configgy.config("data.file"))
    val size = Configgy.config("data.size").toInt
    val metaSize = Configgy.config("data.metasize").toInt
    new MemMapSource(dataFile, size, metaSize)
  }
  lazy val processors = Configgy.config.getInt("processors", Runtime.getRuntime.availableProcessors)
  lazy val serverHost = Configgy.config("server.host")
  lazy val serverPort = Configgy.config("server.port").toInt
  lazy val state = {
    val latest = matrixFolder.list.toList map { _.toLong } sort { _ < _ } last
    val location = new File(matrixFolder, latest.toString)
    val in = new java.io.ObjectInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream(location)))
    try {
      in.readObject.asInstanceOf[ServerState]
    } finally {
      in.close
    }
  }
  lazy val timeout = Configgy.config("timeout").toInt

  def configState = {
    val sample = Configgy.config("state.sample").toDouble
    val steps = Configgy.config("state.steps").toInt
    val epsilon = Configgy.config("state.epsilon").toDouble
    ConfigState(sample, steps, epsilon)
  }
  def clientConfig = {
    val configState = this.configState
    ClientConfig(configState.sample, configState.steps)
  }
}

case class ConfigState(
  sample: Double,
  steps: Int,
  epsilon: Double)

