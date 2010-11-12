package org.no.ip.bca.superlearn

import java.io._
import java.net._
import java.nio._
import java.security._
import java.util._

import org.apache.commons.math.stat.StatUtils
import org.no.ip.bca.scala.Ranges

import se.scalablesolutions.akka.actor.Actor
import Actor._
import se.scalablesolutions.akka.remote.RemoteNode

object MatrixRecorder {
  final case class Record(when: Long, serverState: ServerState)
  final case object Latest
  
  def main(args: Array[String]): Unit = {
    RemoteNode.start(ConfigHelper.hostname, ConfigHelper.matrixRecorderPort)
    RemoteNode.register("matrix-recorder", actorOf[MatrixRecorder])
  }
}

class MatrixRecorder extends Actor {
  import MatrixRecorder._

  def receive = {
    case Record(when, serverState) =>
      val digest = MessageDigest.getInstance("SHA1")
      val out = new ObjectOutputStream(new DigestOutputStream(new BufferedOutputStream(
        new FileOutputStream(new File(ConfigHelper.matrixFolder, when.toString))), digest))
      try {
        out.writeObject(serverState)
        out.write(digest.digest)
      } finally {
        out.close
      }
      val hAct = serverState.compute.hAct
      val vAct = serverState.compute.vAct
      val count = serverState.compute.count.toDouble
      log.info(comp(hAct, count) + " " + comp(vAct, count))
      val hZero = computeZeros(hAct)
      val vZero = computeZeros(vAct)
      log.info("ZEROS!: " + computeZeros(hAct) + " " + computeZeros(vAct))
      if ((hZero - vZero).abs > 0.25) {
        // TODO: Make this a little more sane
        exit(1)
      }
    case Latest => self reply latestState
  }
  
  def computeZeros(v: math.Matrix.ImmutableV[_]) = (0.0 /: v.asArray)((a, b) => if (b == 0) a + 1 else a) / v.size.toDouble

  def comp(a: math.Matrix.ImmutableV[_], count: Double) = {
    val norm = a / count
    val mean = norm.mean
    val variance = norm variance mean
    (mean, variance)
  }
  
  def latestState = {
    val latest = ConfigHelper.matrixFolder.list.toList map { _.toLong } sortWith { _ < _ } last
    val location = new File(ConfigHelper.matrixFolder, latest.toString)
    val in = new java.io.ObjectInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream(location)))
    try {
      in.readObject.asInstanceOf[ServerState]
    } finally {
      in.close
    }
  }
}