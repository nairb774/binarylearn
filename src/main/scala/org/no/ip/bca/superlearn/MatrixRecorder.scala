package org.no.ip.bca.superlearn

import java.io._
import java.nio._
import java.security._
import java.util._

import scala.actors.Actor.actor

import org.no.ip.bca.scala.Ranges

class MatrixRecorder(dir: File) {
  def record(serverState: ServerState) = {
    val when = System.currentTimeMillis
    actor {
      val digest = MessageDigest.getInstance("SHA1")
      val out = new ObjectOutputStream(new BufferedOutputStream(new DigestOutputStream(
        new FileOutputStream(new File(dir, when.toString)), digest)))
      try {
        out.writeObject(serverState)
        out.write(digest.digest)
      } finally {
        out.close
      }
    }
  }
}