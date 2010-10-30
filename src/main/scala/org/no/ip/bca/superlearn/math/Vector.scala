package org.no.ip.bca.superlearn.math

object Vector {
  def withLength[@specialized P <: AnyVal, T](length: Int)(implicit p: ClassManifest[P]) =
    new Vector[P, T](new Array[P](length))
}
@serializable
final class Vector[@specialized P <: AnyVal, T](val v: Array[P])(implicit p: ClassManifest[P]) {
  assert(v.length > 0)
  
  def length = v.length
  def empty = new Vector[P, T](new Array[P](v.length))

  override def toString: String = {
    val sb = new StringBuilder
    sb append ("Vector[" + length + "] {") append v(0)
    var i = 1
    while (i < length) {
      sb append ',' append v(i)
      i += 1
    }
    sb append "}" toString
  }
}