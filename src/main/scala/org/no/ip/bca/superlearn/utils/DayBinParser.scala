package org.no.ip.bca.superlearn.utils

object DayBinParser {
  import java.io._
  import java.nio.channels.FileChannel.MapMode
  import com.google.common.base.Charsets
  import com.google.common.io.Files

  private def using[T <: { def close(): Unit }, A](t: T)(f: T => A) = try { f(t) } finally { t.close() }

  def main(args: Array[String]): Unit = {
    val fIn = new File(args(0))
    val fOut = new File(args(1))
    using(new RandomAccessFile(fOut, "rw")) { _.setLength(822294 * 416) }
    val map = Files.map(fOut, MapMode.READ_WRITE)
    using(new BufferedReader(new InputStreamReader(new FileInputStream(fIn), Charsets.UTF_8))) { f =>
      var line = f.readLine
      while (line != null) {
        map put {
          line.substring(0, 416).toCharArray map { c =>
            if (c == 'T') 1.toByte else 0.toByte
          }
        }
        line = f.readLine
      }
    }
    map.force
  }
}