package org.no.ip.bca.superlearn

import java.io._
import java.nio._

import com.google.common.io.Files

import org.no.ip.bca.scala.Ranges
import math.Matrix

trait DataIterator[S, M] {
  val size: Int
  val metaSize: Int
  val out: Matrix.MutableV[S]
  val hasNext: () => Boolean
  def next: Matrix.MutableV[S]
  val meta: () => Matrix.MutableV[M]
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
  val out = Matrix.mutable.vector[Side.V](size)
  val meta_ = Matrix.mutable.vector[Side.V](metaSize)

  val skip = { () => pos += 1 }
  def next = {
    var offset = pos * totalSize
    pos += 1
    var i = 0
    val outa = out.asArray
    while (i < size) {
      outa(i) = block(offset + i)
      i += 1
    }
    out
  }
  val meta = { () =>
    var offset = pos * totalSize - metaSize // Look at previous meta (assumes that next is called before meta)
    var i = 0
    val metaa = meta_.asArray
    while (i < metaSize) {
      metaa(i) = block(offset + i)
      i += 1
    }
    meta_
  }
  val hasNext = { () => pos < count }
}

class MetaMerge[S](iter: DataIterator[S, S]) extends DataIterator[S, S] {
  val size = iter.size + iter.metaSize
  val metaSize = 0
  val out = Matrix.mutable.vector[S](size)
  val skip = iter.skip
  val hasNext = iter.hasNext
  def next = {
    val merged = new Array[Double](out.size)
    val nexta = iter.next.asArray
    System.arraycopy(nexta, 0, merged, 0, nexta.length)
    val metaa = iter.meta().asArray
    System.arraycopy(metaa, 0, merged, nexta.length, metaa.length)
    // TODO: Fix!
    out.`zero=` += Matrix.mutable.from[S](merged)
    out
  }
  val meta = { () => null }
}
