package org.no.ip.bca.superlearn

import java.io._
import java.nio._

import com.google.common.io.Files

import org.no.ip.bca.scala.Ranges

trait DataIterator extends Iterator[Array[Byte]] {
  val out: Array[Byte]
  def skip: Unit
}

class MemMapSource(file: File, size: Int) {
  private val threadsMap = new ThreadLocal[MappedByteBuffer] {
    override def initialValue = Files.map(file)
  }
  def iter(range: Ranges.Pair) = {
    val map = threadsMap.get
    map limit (range.end * size).toInt
    map position (range.start * size).toInt
    new MemMapWalker(map, size)
  }
}

class MemMapWalker(map: MappedByteBuffer, size: Int) extends DataIterator {
  val out = new Array[Byte](size)
  def skip = map position (map.position + size)
  def next = {
    val o = out
    map.get(o)
    o
  }
  def hasNext = map.hasRemaining
}