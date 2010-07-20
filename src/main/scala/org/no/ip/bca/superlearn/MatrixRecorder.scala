package org.no.ip.bca.superlearn

import java.io._
import java.nio._
import java.security.MessageDigest
import java.util.UUID

import scala.actors.Actor.actor

import org.no.ip.bca.scala.Ranges

class MatrixRecorder(dir: File) {
    def record(matrix: Matrix, error: Double) = {
        val when = System.currentTimeMillis
        println(when + " " + error)
        actor {
            val bytes = new Array[Byte](matrix.w * matrix.h * 8 + 8)
            ByteBuffer.wrap(bytes).asDoubleBuffer.put(matrix.m).put(error)
            val digest = MessageDigest.getInstance("SHA1").digest(bytes)
            val out = new BufferedOutputStream(new FileOutputStream(new File(dir, when.toString)))
            try {
                out.write(bytes)
                out.write(digest)
            } finally {
                out.close
            }
        }
    }
}