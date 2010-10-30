package org.no.ip.bca.superlearn

import org.specs._

case class matchArray[T <: AnyVal](a: Array[T]) extends matcher.Matcher[Array[T]] {
  import java.util.Arrays
  def apply(v: => Array[T]) = {
    val _v = v
    val equal = a zip _v forall { case (a, b) => a == b }
    (equal, "okMessage", _v mkString ("[", ",", "]"))
  }
}
