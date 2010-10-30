package org.no.ip.bca.superlearn

import java.io._
import java.nio._

import com.google.common.io.Files

import org.no.ip.bca.scala.Ranges
import math.Vector
import math.Types._

trait DataIterator[S, M] {
  val size: Int
  val metaSize: Int
  val out: VectorD[S]
  val hasNext: () => Boolean
  def next: VectorD[S]
  val meta: () => VectorD[M]
  val skip: () => Unit
}

class MemMapSource(file: File, size: Int, metaSize: Int) {
  private val map = Files.map(file)

  def iter(range: Ranges.Pair) = {
    val block = new Array[Byte]((size + metaSize) * (range.end - range.start).toInt)
    synchronized {
      map position range.start.toInt
      map.get(block)
    }
    new MemMapWalker(block, size, metaSize)
  }
}

class MemMapWalker(block: Array[Byte], val size: Int, val metaSize: Int) extends DataIterator[Side.V, Side.V] {
  private val totalSize = size + metaSize
  private var pos = 0
  private val count = block.length / totalSize
  val out = Vector.withLength[Double, Side.V](size)
  private val meta_ = Vector.withLength[Double, Side.V](metaSize)

  val skip = { () => pos += 1 }
  def next = {
    var i = 0
    var j = pos * totalSize
    val outv = out.v
    while (i < size) {
      outv(i) = block(j)
      i += 1
      j += 1
    }
    pos += 1

    out
  }
  val meta = { () =>
    var i = 0
    var j = pos * totalSize - metaSize // Look at previous meta (assumes that next is called before meta)
    val meta_v = meta_.v
    while (i < metaSize) {
      meta_v(i) = block(j)
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
  val out = Vector.withLength[Double, S](size)
  val skip = iter.skip
  val hasNext = iter.hasNext
  def next = {
      val nextv = iter.next.v
      System.arraycopy(nextv, 0, out.v, 0, nextv.length)
      val metav = iter.meta().v
      System.arraycopy(metav, 0, out.v, nextv.length, metav.length)
      out
  }
  val meta = { () => null }
}
