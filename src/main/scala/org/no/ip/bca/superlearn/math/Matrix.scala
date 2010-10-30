package org.no.ip.bca.superlearn.math

object Matrix {
  def withSize[@specialized P <: AnyVal, L, R](r: Int, c: Int)(implicit p: ClassManifest[P]) =
    new Matrix[P, L, R](r, c, new Array[P](r * c))
}

@serializable
final class Matrix[@specialized P <: AnyVal, L, R](val rows: Int, val columns: Int, val a: Array[P])(implicit p: ClassManifest[P]) {
  assert(a.length > 0)
  assert(rows > 0)
  assert(columns > 0)
  assert(rows * columns == a.length)
  
  def empty = new Matrix[P, L, R](rows, columns, new Array[P](a.length))
  def elementCount = a.length
  def T = {
    val matrix = this.a
    val elementCount = this.elementCount
    val w = this.rows
    val h = this.columns

    val t = new Matrix[P, R, L](h, w, new Array[P](matrix.length))
    val tM = t.a
    var m = 0
    var y = 0
    var x = 0
    while (m < elementCount) {
      tM(x * h + y) = matrix(m)
      m += 1
      x += 1
      if (x == w) {
        y += 1
        x = 0
      }
    }
    t
  }
  
  override def toString: String = {
    val sb = new StringBuilder
    sb append ("Matrix[" + rows + ":" + columns + "]")
    var i = 0
    var x = rows
    while (i < elementCount) {
      if (x == rows) {
        sb append '\n' append a(i)
        x = 1
      } else {
        sb append ',' append a(i)
        x += 1
      }
      i += 1
    }
    sb.toString
  }
}