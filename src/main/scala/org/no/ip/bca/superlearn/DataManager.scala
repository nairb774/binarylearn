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
    new MemMapWalker(threadsMap.get, size, range)
  }
}

class MemMapWalker(map: MappedByteBuffer, size: Int, range: Ranges.Pair) extends DataIterator {
  val out = new Array[Byte](size)
  private var pos = (range.start * size).toInt
  private val limit = (range.end * size).toInt
  def skip = pos += size
  def next = {
    val o = out
    map position pos
    map.get(o)
    pos += size
    o
  }
  def hasNext = pos < limit
}
