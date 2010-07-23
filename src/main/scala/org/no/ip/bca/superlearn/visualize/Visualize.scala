package org.no.ip.bca.superlearn.visualize

import java.awt.Color.HSBtoRGB
import java.awt.image.BufferedImage
import java.io._
import java.nio._
import javax.imageio.ImageIO

object Visualize {
    def main(args: Array[String]): Unit = {
        val dir = new File(args(0))
        val w = Integer.parseInt(args(1))
        val h = Integer.parseInt(args(2))
        val outDir = new File(args(3))
        val v = new Visualize(dir, w, h, outDir)
        v.writeTopo
        v.makeDiffs
    }
}

class Visualize(dir: File, w: Int, h: Int, outDir: File) {
    def writeTopo = {
        for (matrixFile <- dir.listFiles) {
            ImageIO.write(makeTopo(loadMatrix(matrixFile)), "png", new File(outDir, matrixFile.getName() + ".topo.png"))
        }
    }
    
    def makeDiffs = {
        dir.listFiles.sortWith(_.getName < _.getName) reduceLeft { (a, b) =>
            val matrix = loadMatrix(a)
            FastCode.sub(loadMatrix(b), matrix)
            ImageIO.write(makeTopo(matrix), "png", new File(outDir, a.getName() + ".diff.png"))
            val pw = new PrintWriter(new File(outDir, a.getName() + ".diff.txt"))
            try {
                FastCode.toString(matrix, w, h, pw)
            } finally {
                pw.close
            }
            b
        }
    }
    
    def makeTopo(matrix: Array[Double]) = {
        val minmax = FastCode.minmax(matrix)
        val min = minmax(0)
        val spread = minmax(1) - min
        println(min + " " + minmax(1) + " " + spread)
        val rgb = for (pt <- matrix) yield HSBtoRGB(((pt - min) / spread).toFloat, 1.0f, 1.0f)
        val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
        image.setRGB(0, 0, w, h, rgb, 0, w)
        image
    }
    
    def loadMatrix(file: File): Array[Double] = {
        val bytes = new Array[Byte](w * h * 8)
        val in = new DataInputStream(new FileInputStream(file))
        try {
            in.readFully(bytes)
        } finally {
            in.close
        }
        val pts = new Array[Double](w * h)
        ByteBuffer.wrap(bytes).asDoubleBuffer.get(pts)
        pts
    }
}