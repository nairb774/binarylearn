package org.no.ip.bca.superlearn

import java.io._
import java.nio._

import org.no.ip.bca.scala.Ranges

object DataBlock {
  import com.google.common.io.Files.map
  def readBitmap(file: File, count: Int) = {
    val block = new Array[Byte](count)
    val buffer = map(file)
    buffer.get(block)
    /*val exists = file.exists
    val channel = new RandomAccessFile(file, "rw")
    if (exists) {
      channel.readFully(block)
    } else {
      channel.write(block)
    }*/
    (block, buffer)
  }
}

class DataBlock(file: File, offset: Long, headerSize: Int) {
  private val (bitmap, raf) = DataBlock.readBitmap(file, headerSize)
  private var min = Integer.MAX_VALUE
  private var max = Integer.MIN_VALUE

  def ranges = {
    val l = new scala.collection.mutable.ListBuffer[Ranges.Pair]
    val top = headerSize * 8 + offset
    var i = offset
    var start = -1L
    while (i < top) {
      if (has(i)) {
        if (start < 0) {
          start = i
        }
      } else {
        if (start >= 0) {
          l append Ranges.Pair(start, i)
          start = -1
        }
      }
      i += 1
    }
    if (start >= 0) {
      l append Ranges.Pair(start, i)
    }
    Ranges(l: _*)
  }
  def has(_pos: Long) = {
    val pos = (_pos - offset).toInt
    (bitmap(pos / 8) & (1 << (pos & 7))) != 0
  }
  def read(_pos: Long, block: Array[Byte]) = {
    val pos = _pos - offset
    raf position (headerSize + pos * block.length).toInt
    //raf.seek(headerSize + pos * block.length)
    raf.get(block)
  }
  def write(_pos: Long, block: Array[Byte]) = {
    val pos = (_pos - offset)
    raf position (headerSize + pos * block.length).toInt
    raf.put(block)

    val bytePos = (pos / 8).toInt
    bitmap(bytePos) = (bitmap(bytePos) | (1 << (pos & 7))).asInstanceOf[Byte]
    if (bytePos < min) min = bytePos
    if (bytePos > max) max = bytePos
  }
  def flush = {
    if (min <= max) {
      raf position (min)
      raf.put(bitmap, min, max - min + 1)
      val count = max - min
      min = Integer.MAX_VALUE
      max = Integer.MIN_VALUE
      count
    } else 0
  }
  def close = try {
    flush
  } finally {
    raf.force
  }
}

class DataBlockPoolFactory(root: File, headerSize: Int) extends org.apache.commons.pool.BaseKeyedPoolableObjectFactory {
  def makeObject(_key: AnyRef) = {
    val key = _key.asInstanceOf[Long]
    new DataBlock(new File(root, key.toString), key, headerSize)
  }
  override def destroyObject(_key: AnyRef, _value: AnyRef) = {
    val value = _value.asInstanceOf[DataBlock]
    value.close
  }
}

class DataManager(val folder: File, val size: Int, val max: Long, spans: Int) {
  import java.util.TreeSet
  assert((spans & 7) == 0)
  private var blocks = {
    val ret = new org.apache.commons.pool.impl.GenericKeyedObjectPool(new DataBlockPoolFactory(folder, spans / 8))
    ret setMaxActive 1 // per key
    ret setMaxTotal 250
    ret setTimeBetweenEvictionRunsMillis (5 * 60 * 1000)
    ret
  }
  private val _rangesLock = new Object
  private var _ranges = {
    var i = 0L
    var ranges = Ranges()
    while (i < max) {
      ranges |= getDataBlock(i) { _.ranges }
      i += spans
    }
    ranges
  }
  private def getDataBlock[T](pos: Long)(f: DataBlock => T) = {
    var floor = pos / spans * spans
    val block = blocks.borrowObject(floor).asInstanceOf[DataBlock]
    try {
      f(block)
    } finally {
      blocks.returnObject(floor, block)
    }
  }
  def has(pos: Long) = getDataBlock(pos) { _.has(pos) }
  def ranges = _rangesLock.synchronized { _ranges }
  def read(pos: Long): Array[Byte] = {
    val block = new Array[Byte](size)
    read(pos, block)
    block
  }
  def read(pos: Long, block: Array[Byte]) = getDataBlock(pos) { _.read(pos, block) }
  def write(pos: Long, block: Array[Byte]) = {
    getDataBlock(pos) { _.write(pos, block) }
    _rangesLock.synchronized { _ranges |= pos }
  }
  def iter(pair: Ranges.Pair): DataIterator = iter(pair.start, pair.end)
  def iter(start: Long, end: Long): DataIterator = new DataIteratorImpl(this, start, end, size)
  def clear = blocks.clear
  def close = blocks.close
}

trait DataIterator extends Iterator[Array[Byte]] {
  val out: Array[Byte]
  def skip: Unit
}

class DataIteratorImpl(manager: DataManager, start: Long, end: Long, size: Int) extends DataIterator {
  val out = new Array[Byte](size)
  private var pos = start
  def hasNext = pos < end
  def next = {
    manager.read(pos, out)
    pos += 1
    out
  }
  def skip = {
    pos += 1
  }
}