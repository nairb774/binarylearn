package org.no.ip.bca.superlearn
package utils

import java.awt.image._
import java.io._
import java.util._
import javax.imageio._

import scala.math._

import net.lag.configgy.Configgy

import org.no.ip.bca.scala.Ranges
import math._

object MatrixStats {
  def main(args: Array[String]): Unit = {
    Configgy configure args(0)
    //calcWeights
    //println(">>")
    project
    makeImages("cd-", { f => f.compute.cd | { _ / f.compute.count.toDouble } })
    makeImages("weights-", f => f.state.weights)
    ConfigHelper.matrixFolder.list.toList map { _.toLong } sortWith { _ < _ } drop 1 foreach { f =>
      println((0 /: loadMatrix(f).compute.vAct.asArray)((c, v) => if (v == 0.0) c + 1 else c))
      println((0 /: loadMatrix(f).compute.hAct.asArray)((c, v) => if (v == 0.0) c + 1 else c))
    }
  }
  
  def calcWeights = {
    val list = ConfigHelper.matrixFolder.list.toList map { _.toLong } sortWith { _ < _ } drop 1
    val weights = Matrix.immutable.matrix[Side.H, Side.V](500, 28 * 28)
    (weights /: list) { (weights, id) =>
      val matrix = loadMatrix(id)
      val count = matrix.compute.count.toDouble
      val cd = matrix.compute.cd / count
      val angle = weights angle cd
      println(180.0 * angle / Pi)
      weights * 0.9 + cd * 0.1
    }
  }
  
  def project = {
    val list = ConfigHelper.matrixFolder.list.toList map { _.toLong } sortWith { _ < _ }
    list.zipWithIndex foreach {
      case (f, index) =>
        val matrix = loadMatrix(f)
        val iter = ConfigHelper.memMapSource.iter(Ranges.Pair(70, 71))
        val worker = MatrixWorker.builder(iter).worker(matrix.state.toMutable, ClientConfig(matrix.configState.sample, matrix.configState.steps))
        worker.root.next
        val bimg = new BufferedImage(28, 28, BufferedImage.TYPE_INT_RGB)
        val data = worker.v1.asArray map { i => if (i == 1) 0xFFFFFF else 0 }
        bimg.setRGB(0, 0, 28, 28, data, 0, 28)
        var indexString = index.toString
        while (indexString.length < 4) indexString = "0" + indexString
        ImageIO.write(bimg, "png", new File(ConfigHelper.imageFolder, "project-" + indexString + ".png"))
    }
  }

  def makeImages(prefix: String, func: ServerState => CommonsMatrix[_, _]) = {
    val list = ConfigHelper.matrixFolder.list.toList map { _.toLong } sortWith { _ < _ } drop 1
    val min = list map { f =>
      val i = func(loadMatrix(f)).reduce { _ min _ }
      println(i)
      i
    } reduceLeft { _ min _ }
    println("#")
    val max = list map { f =>
      val i = func(loadMatrix(f)).reduce { _ max _ }
      println(i)
      i
    } reduceLeft { _ min _ }
    println("#")
    val slope = 255.0 / (max - min)
    println(slope)
    list.zipWithIndex drop 0 foreach {
      case (f, index) =>
        val matrix = func(loadMatrix(f))
        val img = new BufferedImage(matrix.columns, matrix.rows, BufferedImage.TYPE_INT_RGB)
        matrix foreach { (x, y, v) =>
          val ii = ((v - min) * slope).toInt
          img.setRGB(x, y, (ii << 16) | (ii << 8) | ii)
        }
        var indexString = index.toString
        while (indexString.length < 4) indexString = "0" + indexString
        ImageIO.write(img, "png", new File(ConfigHelper.imageFolder, prefix + indexString + ".png"))
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