package org.no.ip.bca.superlearn.visualize

import java.awt.Color.HSBtoRGB
import java.awt.image.BufferedImage
import java.io._
import java.nio._
import javax.imageio.ImageIO

import org.no.ip.bca.superlearn._

object Visualize {
    def main(args: Array[String]): Unit = {
        testFastRandom
        exit(0)
        val dir = new File(args(0))
        val w = Integer.parseInt(args(1))
        val h = Integer.parseInt(args(2))
        val outDir = new File(args(3))
        val v = new Visualize(dir, w, h, outDir)
        v.writeTopo
        //v.makeDiffs
    }
    
    def testFastRandom = {
        val fr = new FastRandom
        val bins = new Array[Long](10)
        var i = 0
        while (i < 100000) {
            val d = fr.nextDouble
            var d_ = 0.1
            var j = 0
            while (d >= j) {
                d_ += 0.1
                j += 1
            }
            bins(j) += 1
        }
        println(bins mkString " ")
    }
}

class Visualize(dir: File, w: Int, h: Int, outDir: File) {
    def writeTopo = {
        for (matrixFile <- dir.listFiles.sortWith(_.getName < _.getName)) {
            val topoImage = makeTopo(loadMatrix(matrixFile))
            //ImageIO.write(topoImage, "png", new File(outDir, matrixFile.getName() + ".topo.png"))
        }
    }
    
    /*def makeDiffs = {
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
    }*/
    
    def makeTopo(state: State) = {
        val minmax = FastCode.minmax(state.weights)
        val minmaxV = FastCode.minmax(state.visible)
        val minmaxH = FastCode.minmax(state.hidden)
        val min = minmax(0)
        val spread = minmax(1) - min
        println(List(min, minmax(1), spread, minmaxV(0), minmaxV(1), minmaxV(1) - minmaxV(0), minmaxH(0), minmaxH(1), minmaxH(1) - minmaxH(0)) mkString " ")
        /*val rgb = for (pt <- state.weights) yield HSBtoRGB(((pt - min) / spread).toFloat, 1.0f, 1.0f)
        val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
        image.setRGB(0, 0, w, h, rgb, 0, w)
        image*/
    }
    
    def loadMatrix(file: File) = {
        val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(file)))
        val ss = try {
            in.readObject.asInstanceOf[ServerState]
        } finally {
            in.close
        }
        ss.state
    }
}