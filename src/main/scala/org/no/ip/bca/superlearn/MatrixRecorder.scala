package org.no.ip.bca.superlearn

import java.io._
import java.net._
import java.nio._
import java.security._
import java.util._

import scala.actors.Actor.actor

import net.lag.configgy.Configgy
import org.apache.commons.math.stat.StatUtils
import org.no.ip.bca.scala.Ranges

object MatrixRecorder {
  def main(args: Array[String]): Unit = {
    Configgy configure args(0)
    val ss = new ServerSocket(ConfigHelper.matrixRecorderPort)
    val s = ss.accept
    s.setSoTimeout(ConfigHelper.timeout)
    try {
      handleSocket(s)
    } finally {
      s.close
    }
  }

  def handleSocket(s: Socket) = {
    val out = new ObjectOutputStream(new BufferedOutputStream(s.getOutputStream))
    out.writeObject(latestState)
    out.reset
    out.flush

    val in = new ObjectInputStream(new BufferedInputStream(s.getInputStream))
    while (true) {
      val state = in.readObject.asInstanceOf[ServerState]
      val now = System.currentTimeMillis
      actor { handleState(now, state) }
    }
  }

  def handleState(when: Long, serverState: ServerState) = {
    val digest = MessageDigest.getInstance("SHA1")
    val out = new ObjectOutputStream(new BufferedOutputStream(new DigestOutputStream(
      new FileOutputStream(new File(ConfigHelper.matrixFolder, when.toString)), digest)))
    try {
      out.writeObject(serverState)
      out.write(digest.digest)
    } finally {
      out.close
    }
    val hAct = serverState.compute.hAct.a
    val vAct = serverState.compute.vAct.a
    val count = serverState.compute.count.toDouble
    println(comp(hAct, count) + " " + comp(vAct, count))
    val hZero = (0.0 /: hAct)((a, b) => if (b == 0) a + 1 else a) / hAct.length.toDouble
    val vZero = (0.0 /: vAct)((a, b) => if (b == 0) a + 1 else a) / vAct.length.toDouble
    println("ZEROS!: " + hZero + " " + vZero)
    if ((hZero - vZero).abs > 0.25) {
      exit(1)
    }
  }

  def comp(a: Array[Double], count: Double) = {
    val norm = a map { _ / count }
    val mean = StatUtils.mean(norm)
    val variance = StatUtils.variance(norm, mean)
    (mean, variance)
  }

  def latestState = {
    val latest = ConfigHelper.matrixFolder.list.toList map { _.toLong } sort { _ < _ } last
    val location = new File(ConfigHelper.matrixFolder, latest.toString)
    val in = new java.io.ObjectInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream(location)))
    try {
      in.readObject.asInstanceOf[ServerState]
    } finally {
      in.close
    }
  }
}

class MatrixRecorder {
  val (in, out) = {
    val s = new Socket(ConfigHelper.matrixRecorderHost, ConfigHelper.matrixRecorderPort)
    s.setSoTimeout(ConfigHelper.timeout)
    val in = new ObjectInputStream(new BufferedInputStream(s.getInputStream))
    val out = new ObjectOutputStream(new BufferedOutputStream(s.getOutputStream))
    (in, out)
  }
  
  def latestState = {
    in.readObject.asInstanceOf[ServerState]
  }
  
  def record(serverState: ServerState) = {
    actor {
      try {
        out.writeObject(serverState)
        out.reset
        out.flush
      } catch {
        case e =>
          e.printStackTrace()
          exit(1)
      }
    }
  }
}