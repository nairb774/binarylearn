package org.no.ip.bca.superlearn

import java.io._
import java.nio._

import com.google.common.io.Files

import org.no.ip.bca.scala.Ranges
import math._

trait DataIterator[S, M] {
  val size: Int
  val metaSize: Int
  val out: RVector[S]
  val hasNext: () => Boolean
  def next: RVector[S]
  val meta: () => RVector[M]
  val skip: () => Unit
}

class MemMapSource(file: File, size: Int, metaSize: Int) {
  private val map = Files.map(file)
  private val totalSize = size + metaSize

  def at(point: Int) = totalSize * point
  def at(point: Long) = totalSize * point
  def iter(range: Ranges.Pair) = {
    val block = new Array[Byte]((at(range.end) - at(range.start)).toInt)
    synchronized {
      map position at(range.start).toInt
      map.get(block)
    }
    new MemMapWalker(block, size, metaSize)
  }
}

class MemMapWalker(block: Array[Byte], val size: Int, val metaSize: Int) extends DataIterator[Side.V, Side.V] {
  private val totalSize = size + metaSize
  private var pos = 0
  private val count = block.length / totalSize
  val out = RVector.withLength[Side.V](size)
  private val meta_ = RVector.withLength[Side.V](metaSize)

  val skip = { () => pos += 1 }
  def next = {
    var i = 0
    var j = pos * totalSize
    val outa = out.a
    while (i < size) {
      outa(i) = block(j)
      i += 1
      j += 1
    }
    pos += 1

    out
  }
  val meta = { () =>
    var i = 0
    var j = pos * totalSize - metaSize // Look at previous meta (assumes that next is called before meta)
    val meta_a = meta_.a
    while (i < metaSize) {
      meta_a(i) = block(j)
      i += 1
      j += 1
    }
    meta_
  }
  val hasNext = { () => pos < count }
}

class MetaMerge[S](iter: DataIterator[S, S]) extends DataIterator[S, S] {
  val size = iter.size + iter.metaSize
  val metaSize = 0
  val out = RVector.withLength[S](size)
  val skip = iter.skip
  val hasNext = iter.hasNext
  def next = {
      val nexta = iter.next.a
      System.arraycopy(nexta, 0, out.a, 0, nexta.length)
      val metaa = iter.meta().a
      System.arraycopy(metaa, 0, out.a, nexta.length, metaa.length)
      out
  }
  val meta = { () => null }
}
