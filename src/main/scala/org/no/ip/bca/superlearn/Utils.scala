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
        val props = new Props(args(0))
        val w = props.w
        val h = props.h
        val len = w * h
        val weights = new Array[Double](len)
        val visible = new Array[Double](w)
        val hidden = new Array[Double](h)
        var i = 0
        while (i < len) {
            weights(i) = 0.01 * random.nextDouble - 0.005
            i += 1
        }
        val state = State(weights, hidden, visible, 0.05, 1)
        val momentum = Momentum(new Array(len), new Array(w), new Array(h))
        val ss = ServerState(state, momentum)
        props.matrixRecorder.record(ss, 0.0)
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
    
    lazy val matrixRecorder = {
        val folder = new File(props.getProperty("matrix.folder"))
        new MatrixRecorder(folder)
    }
    
    lazy val w = Integer.parseInt(props.getProperty("matrix.w"))
    lazy val h = Integer.parseInt(props.getProperty("matrix.h"))
    
    lazy val state = {
        val location = new File(props.getProperty("matrix.location"))
        val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(location)))
        try {
            in.readObject.asInstanceOf[ServerState]
        } finally {
            in.close
        }
    }
}