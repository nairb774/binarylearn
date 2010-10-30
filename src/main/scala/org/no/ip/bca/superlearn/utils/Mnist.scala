package org.no.ip.bca.superlearn.utils

object MinstDataShow {
  import java.awt.image.BufferedImage
  import java.io.File
  import com.google.common.io.Files

  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val map = Files.map(file)
    map position ((28 * 28 + 10) * 500)
    val img = new Array[Byte](28 * 28)
    map get img
    val bimg = new BufferedImage(28, 28, BufferedImage.TYPE_INT_RGB)
    val data = img map { i => if (i == 1) 0xFFFFFF else 0 }
    bimg.setRGB(0, 0, 28, 28, data, 0, 28)
    javax.imageio.ImageIO.write(bimg, "png", new File(args(1)))
    val i2 = new Array[Byte](10)
    map get i2
    println(java.util.Arrays.toString(i2))
  }
}

object MnistImageParser {
  import java.io.File
  import java.nio.channels.FileChannel.MapMode
  import com.google.common.io.Files

  def main(args: Array[String]): Unit = {
    val fIn = new File(args(0))
    val fOut = new File(args(1))

    if (!fOut.exists) {
      fOut.createNewFile
    }

    val inMap = Files.map(fIn)
    val outMap = Files.map(fOut, MapMode.READ_WRITE)

    val intMapBuffer = inMap.asIntBuffer
    assert(intMapBuffer.get == 2051)
    intMapBuffer position 1
    val count = intMapBuffer.get
    val size = intMapBuffer.get * intMapBuffer.get
    inMap position (intMapBuffer.position * 4)

    val metaSize = 10
    for (i <- 0 until count) {
      val byteBuffer = new Array[Byte](size)
      val outBuffer = new Array[Byte](size)
      inMap get byteBuffer
      var i = 0
      while (i < size) {
        outBuffer(i) = if (byteBuffer(i) < 0) 1 else 0
        i += 1
      }
      outMap.put(outBuffer)
      outMap position (outMap.position + metaSize)
    }

    outMap.force
  }
}

object MnistLabelParser {
  import java.io.File
  import java.nio.channels.FileChannel.MapMode
  import com.google.common.io.Files

  def main(args: Array[String]): Unit = {
    val fIn = new File(args(0))
    val fOut = new File(args(1))

    if (!fOut.exists) {
      fOut.createNewFile
    }

    val inMap = Files.map(fIn)
    val outMap = Files.map(fOut, MapMode.READ_WRITE)

    val intMapBuffer = inMap.asIntBuffer
    assert(intMapBuffer.get == 2049)
    intMapBuffer position 1
    val count = intMapBuffer.get
    val size = 28 * 28
    inMap position (intMapBuffer.position * 4)

    for (i <- 0 until count) {
      val doubleBuffer = new Array[Byte](10)
      doubleBuffer(inMap.get) = 1
      outMap position (outMap.position + size)
      outMap.put(doubleBuffer)
    }

    outMap.force
  }
}