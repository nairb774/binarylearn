package org.no.ip.bca.superlearn

object DayBinParser {
  def main(args: Array[String]): Unit = {
    val props = new Props(args(0))
    val source = scala.io.Source.fromFile(args(1))
    val dataManager = props.dataManager
    val size = dataManager.size
    source.getLines.zipWithIndex foreach {
      case (line, i) =>
        val out = line.toCharArray.take(size) map { c =>
          if (c == 'T') 1.toByte else 0.toByte
        }
        dataManager.write(i, out)
    }
    dataManager.close
  }
}

object MatrixTextWriter {
  import java.io._
  import java.nio._
  import java.nio.channels.FileChannel.MapMode
  import java.security._
  import java.util._

  import com.google.common.io.Files

  def main(args: Array[String]): Unit = {
    val digest = MessageDigest.getInstance("SHA1")
    val inStream = new ObjectInputStream(new BufferedInputStream(new DigestInputStream(new FileInputStream(args(0)), digest)))
    val serverState = try { inStream.readObject.asInstanceOf[ServerState] } finally { inStream.close }
    val state = serverState.state
    val outFile = new File(args(1))
    val r = new RandomAccessFile(outFile, "rw")
    r.setLength((1 + state.weights.length + state.hidden.length + state.visible.length) * 8)
    r.close
    val map = Files.map(outFile, MapMode.READ_WRITE)
    map.asIntBuffer.put(state.hidden.length).put(state.visible.length)
    map.position(8)
    map.asDoubleBuffer.put(state.weights).put(state.hidden).put(state.visible)
    map.force
    println(minMax(state.weights))
    println(minMax(state.hidden))
    println(minMax(state.visible))
  }

  private def minMax(a: Array[Double]) = {
    var min = a(0)
    var max = a(0)
    val l = a.length
    var i = 1
    while (i < l) {
      val aa = a(i)
      if (aa < min) {
        min = aa
      }
      if (aa > max) {
        max = aa
      }
      i += 1
    }
    (min, max)
  }
}

object MatrixWriter {
  import java.io._
  import java.nio._
  import java.util._
  def main(args: Array[String]): Unit = {
    val random = new FastRandom
    val props = new Props(args(0))
    val w = props.w
    val h = props.h
    val len = w * h
    val weights = new Array[Double](len)
    val visible = new Array[Double](w)
    val hidden = new Array[Double](h)
    var i = 0
    while (i < len) {
      weights(i) = 0.01 * random.nextDouble - 0.005
      i += 1
    }
    val state = State(weights, hidden, visible)
    val momentum = Momentum(new Array(len), new Array(w), new Array(h))
    val score = Score(Double.NaN, Double.NaN, Double.NaN)
    val ss = ServerState(state, momentum, score)
    props.matrixRecorder.record(ss)
  }
}

class Props(file: java.io.File) {
  import java.io._
  import java.nio._
  import java.util._

  def this(_file: String) = this(new java.io.File(_file))

  val props = {
    val reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)), "UTF-8")
    try {
      val props = new Properties
      props.load(reader)
      props
    } finally {
      reader.close
    }
  }

  lazy val timeout = Integer.parseInt(props.getProperty("timeout"))

  lazy val serverHost = props.getProperty("server.host")
  lazy val serverPort = Integer.parseInt(props.getProperty("server.port"))
  lazy val bridgePort = Integer.parseInt(props.getProperty("bridge.port"))
  lazy val processors = {
    val prop = props.getProperty("processors")
    if (prop == null) {
      Runtime.getRuntime.availableProcessors
    } else {
      Integer.parseInt(prop)
    }
  }

  lazy val dataManager = {
    val dataFolder = new File(props.getProperty("data.folder"))
    val size = Integer.parseInt(props.getProperty("data.size"))
    val max = java.lang.Long.parseLong(props.getProperty("data.max"))
    val spans = Integer.parseInt(props.getProperty("data.spans"))
    new DataManager(dataFolder, size, max, spans)
  }

  lazy val matrixRecorder = {
    val folder = new File(props.getProperty("matrix.folder"))
    new MatrixRecorder(folder)
  }

  lazy val w = Integer.parseInt(props.getProperty("matrix.w"))
  lazy val h = Integer.parseInt(props.getProperty("matrix.h"))

  lazy val state = {
    val location = new File(props.getProperty("matrix.location"))
    val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(location)))
    try {
      in.readObject.asInstanceOf[ServerState]
    } finally {
      in.close
    }
  }
}