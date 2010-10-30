package org.no.ip.bca.superlearn
package utils

import java.awt.image._
import java.io._
import java.util._
import javax.imageio._

import net.lag.configgy.Configgy

object MatrixStats {
  def main(args: Array[String]): Unit = {
    Configgy configure args(0)
    makeImages("cd-", 28 * 28, 28 * 28 / 2, { f => f.compute.cd.a map { _ / f.compute.count.toDouble }})
    makeImages("weights-", 28 * 28, 28 * 28 / 2, f => f.state.weights.a)
  }

  def makeImages(prefix: String, w: Int, h: Int, func: ServerState => Array[Double]) = {
    val list = ConfigHelper.matrixFolder.list.toList map { (_.toLong) } sort { _ < _ } drop 1
    val min = list map { f =>
      val i = func(loadMatrix(f)).reduceLeft { _ min _ }
      println(i)
      i
    } reduceLeft { _ min _ }
    println("#")
    val max = list map { f =>
      val i = func(loadMatrix(f)).reduceLeft { _ max _ }
      println(i)
      i
    } reduceLeft { _ min _ }
    println("#")
    val slope = 255.0 / (max - min)
    println(slope)
    list.zipWithIndex foreach {
      case (f, i) =>
        val imgParts = func(loadMatrix(f)).map { v => val ii = ((v - min) * slope).toInt; (ii << 16) | (ii << 8) | ii }
        val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
        img.setRGB(0, 0, w, h, imgParts, 0, w)
        var iii = i.toString
        while (iii.length < 4) iii = "0" + iii
        ImageIO.write(img, "png", new File(ConfigHelper.imageFolder, prefix + iii + ".png"))
    }
  }

  def loadMatrix(id: Long) = {
    val location = new File(ConfigHelper.matrixFolder, id.toString)
    val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(location)))
    try {
      in.readObject.asInstanceOf[ServerState]
    } finally {
      in.close
    }
  }
}