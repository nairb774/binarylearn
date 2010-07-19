package org.no.ip.bca.superlearn

import java.io._
import java.nio._

import org.no.ip.bca.scala.Ranges

object DataBlock {
  def readBitmap(file: File, count: Int) = {
    val block = new Array[Byte](count)
    if (file.exists) {
      val channel = new RandomAccessFile(file, "rw")
      try {
        channel.readFully(block)
      } finally {
        channel.close
      }
    } else {
      val channel = new RandomAccessFile(file, "rw")
      try {
        channel.write(block)
      } finally {
        channel.close
      }
    }
    block
  }
}

class DataBlock(file: File, offset: Long, headerSize: Int) {
  private val bitmap = DataBlock.readBitmap(file, headerSize)
  private var min = Integer.MAX_VALUE
  private var max = Integer.MIN_VALUE
  private val raf = new RandomAccessFile(file, "rw")
  
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
    raf.seek(headerSize + pos * block.length)
    raf.readFully(block)
  }
  def write(_pos: Long, block: Array[Byte]) = {
    val pos = (_pos - offset)
    raf.seek(headerSize + pos * block.length)
    raf.write(block)
    
    val bytePos = (pos / 8).toInt
    bitmap(bytePos) = (bitmap(bytePos) | (1 << (pos & 7))).asInstanceOf[Byte]
    if (bytePos < min) min = bytePos
    if (bytePos > max) max = bytePos
  }
  def flush = {
    if (min <= max) {
      raf.seek(min)
      raf.write(bitmap, min, max - min + 1)
      val count = max - min
      min = Integer.MAX_VALUE
      max = Integer.MIN_VALUE
      count
    } else 0
  }
  def close = try {
    flush
  } finally {
    raf.close
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
    ret setMaxTotal 25
    ret setTimeBetweenEvictionRunsMillis (5 * 60 * 1000)
    ret
  }
  private val _rangesLock = new Object
  private var _ranges = {
    var i = 0L
    var ranges = Ranges()
    while (i < max) {
      ranges |= getDataBlock(i){_.ranges}
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
  def has(pos: Long) = getDataBlock(pos){_.has(pos)}
  def ranges = _rangesLock.synchronized { _ranges }
  def read(pos: Long): Array[Byte] = {
    val block = new Array[Byte](size)
    read(pos, block)
    block
  }
  def read(pos: Long, block: Array[Byte]) = getDataBlock(pos){_.read(pos, block)}
  def write(pos: Long, block: Array[Byte]) = {
    getDataBlock(pos){_.write(pos, block)}
    _rangesLock.synchronized { _ranges |= pos }
  }
  def iter(start: Long, end: Long) = new DataIterator(this, start, end, size)
  def clear = blocks.clear
  def close = blocks.close
}

class DataIterator(manager: DataManager, start: Long, end: Long, size: Int) extends Iterator[Array[Byte]] {
  private val array = new Array[Byte](size)
  private var pos = start
  def hasNext = pos < end
  def next = {
    manager.read(pos, array)
    pos += 1
    array
  }
  def skip = {
    pos += 1
  }
}