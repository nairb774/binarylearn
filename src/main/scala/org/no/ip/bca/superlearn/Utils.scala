package org.no.ip.bca.superlearn

import org.no.ip.bca.scala.{ FastRandom, Ranges }

object DayBinParser {
  import java.io._
  import java.nio.channels.FileChannel.MapMode
  import com.google.common.base.Charsets
  import com.google.common.io.Files

  private def using[T <: { def close(): Unit }, A](t: T)(f: T => A) = try { f(t) } finally { t.close() }

  def main(args: Array[String]): Unit = {
    val fIn = new File(args(0))
    val fOut = new File(args(1))
    using(new RandomAccessFile(fOut, "rw")) { _.setLength(822294 * 416) }
    val map = Files.map(fOut, MapMode.READ_WRITE)
    using(new BufferedReader(new InputStreamReader(new FileInputStream(fIn), Charsets.UTF_8))) { f =>
      var line = f.readLine
      while (line != null) {
        map put {
          line.substring(0, 416).toCharArray map { c =>
            if (c == 'T') 1.toByte else 0.toByte
          }
        }
        line = f.readLine
      }
    }
    map.force
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
    val props = new Props(new File(args(0)))
    props.matrixFolder.listFiles.toList filter { f =>
      !(f.getName endsWith ".binary")
    } sortWith { (a, b) =>
      a.getName < b.getName
    } map { f =>
      convert(f, new File(f.getParentFile, f.getName + ".binary"))
    }
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
  private def convert(f: java.io.File, outFile: java.io.File): Unit = {
    val digest = MessageDigest.getInstance("SHA1")
    val inStream = new ObjectInputStream(new BufferedInputStream(new DigestInputStream(new FileInputStream(f), digest)))
    val serverState = try { inStream.readObject.asInstanceOf[ServerState] } finally { inStream.close }
    val state = serverState.state
    val r = new RandomAccessFile(outFile, "rw")
    r.setLength((1 + state.weights.length + state.hidden.length + state.visible.length) * 8)
    r.close
    val map = Files.map(outFile, MapMode.READ_WRITE)
    map.asIntBuffer.put(state.hidden.length).put(state.visible.length)
    map.position(8)
    map.asDoubleBuffer.put(state.weights).put(state.hidden).put(state.visible)
    map.force
    println((f, minMax(state.weights), minMax(state.hidden), minMax(state.visible)))
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
    i = 0
    while (i < w) {
      visible(i) = 0.01 * random.nextDouble - 0.005
      i += 1
    }
    i = 0
    while (i < h) {
      hidden(i) = 0.01 * random.nextDouble - 0.005
      i += 1
    }
    val state = State(weights, hidden, visible)
    val momentum = Momentum(new Array(len), new Array(h), new Array(w))
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

  lazy val fullRange = Ranges(0, Integer.parseInt(props.getProperty("data.max")))

  lazy val memMapSource = {
    val dataFile = new File(props.getProperty("data.file"))
    val size = Integer.parseInt(props.getProperty("data.size"))
    new MemMapSource(dataFile, size)
  }

  lazy val matrixFolder = new File(props.getProperty("matrix.folder"))
  lazy val matrixRecorder = new MatrixRecorder(matrixFolder)

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