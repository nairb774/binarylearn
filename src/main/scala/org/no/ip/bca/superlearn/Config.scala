package org.no.ip.bca.superlearn

import javax.management.ObjectName
import java.lang.management.ManagementFactory
import java.io.File
import scala.reflect.BeanProperty
import se.scalablesolutions.akka.config.Config.config
import org.no.ip.bca.scala.Ranges
import se.scalablesolutions.akka.remote.RemoteClient

object ConfigHelper {
  config("akka.remote.server.hostname") = java.net.InetAddress.getLocalHost.getCanonicalHostName
  config("akka.remote.server.port") = {
    val ss = new java.net.ServerSocket(0)
    try {
      ss.getLocalPort.toString
    } finally {
      ss.close
    }
  }
  def bootstrap = {}

  lazy val hostname = config("akka.remote.server.hostname")
  lazy val akkaPort = config("akka.remote.server.port").toInt
  lazy val fullRange = Ranges(0, config("data.max").toInt)
  lazy val imageFolder = new File(config("image.folder"))
  lazy val matrixFolder = new File(config("matrix.folder"))
  lazy val matrixRecorder = RemoteClient.actorFor("matrix-recorder", matrixRecorderHost, matrixRecorderPort)
  lazy val matrixRecorderHost = config("recorder.host")
  lazy val matrixRecorderPort = config("recorder.port").toInt
  lazy val memMapSource = {
    val dataFile = new File(config("data.file"))
    val size = config("data.size").toInt
    val metaSize = config("data.metasize").toInt
    new MemMapSource(dataFile, size, metaSize)
  }
  lazy val processors = config.getInt("processors", Runtime.getRuntime.availableProcessors)
  lazy val serverHost = config("server.host")
  lazy val serverPort = config("server.port").toInt
  lazy val timeout = config("timeout").toInt

  def configState = {
    val momentumMix = config("state.momentummix").toDouble
    val sample = config("state.sample").toDouble
    val steps = config("state.steps").toInt
    val epsilon = config("state.epsilon").toDouble
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

