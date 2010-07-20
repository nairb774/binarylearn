package org.no.ip.bca.superlearn

object DayBinParser {
    def main(args: Array[String]): Unit = {
        val props = new Props(args(0))
        val source = scala.io.Source.fromFile(args(1))
        val dataManager = props.dataManager
        val size = dataManager.size
        source.getLines.zipWithIndex foreach { case (line, i) =>
            val out = line.toCharArray.take(size) map { c =>
                if (c == 'T') 1.toByte else 0.toByte
            }
            dataManager.write(i, out)
        }
        dataManager.close
    }
}

object MatrixWriter {
    import java.io._
    import java.nio._
    import java.util._
    def main(args: Array[String]): Unit = {
        val random = new FastRandom
        val len = Integer.parseInt(args(0))
        val m = new Array[Double](len)
        var i = 0
        while (i < len) {
            m(i) = random.nextDouble
            i += 1
        }
        val bytes = new Array[Byte](len * 8)
        ByteBuffer.wrap(bytes).asDoubleBuffer.put(m)
        val out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(args(1))))
        out.write(bytes)
        out.close
    }
}

class Props(file: java.io.File) {
    import java.io._
    import java.nio._
    import java.util._
    
    def this(_file: String) = this(new java.io.File(_file))
    
    val props = {
        val reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)), "UTF-8")
        try {
            val props = new Properties
            props.load(reader)
            props
        } finally {
          reader.close
        }
    }
    
    lazy val timeout = 1000 * 1000
    
    lazy val serverHost = props.getProperty("server.host")
    lazy val serverPort = Integer.parseInt(props.getProperty("server.port"))
    lazy val bridgePort = Integer.parseInt(props.getProperty("bridge.port"))
    lazy val processors = {
        val prop = props.getProperty("processors")
        if (prop == null) {
            Runtime.getRuntime.availableProcessors
        } else {
            Integer.parseInt(prop)
        }
    }
    
    lazy val dataManager = {
        val dataFolder = new File(props.getProperty("data.folder"))
        val size = Integer.parseInt(props.getProperty("data.size"))
        val max = java.lang.Long.parseLong(props.getProperty("data.max"))
        val spans = Integer.parseInt(props.getProperty("data.spans"))
        new DataManager(dataFolder, size, max, spans)
    }
    
    lazy val matrix = {
        val location = new File(props.getProperty("matrix.location"))
        val w = Integer.parseInt(props.getProperty("matrix.w"))
        val h = Integer.parseInt(props.getProperty("matrix.h"))
        val bytes = new Array[Byte](w * h * 8)
        val in = new DataInputStream(new BufferedInputStream(new FileInputStream(location)))
        try {
            in.readFully(bytes)
        } finally {
            in.close
        }
        val m = new Array[Double](w * h)
        ByteBuffer.wrap(bytes).asDoubleBuffer.get(m)
        Matrix(w, h, m)
    }
}