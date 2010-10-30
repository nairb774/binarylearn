package org.no.ip.bca.superlearn.math

import org.specs._

case class matchMatrix[P <: AnyVal, L, R](a: Matrix[P, L, R]) extends matcher.Matcher[Matrix[P, L, R]] {
  import java.util.Arrays
  def apply(v: => Matrix[P, L, R]) = {
    val _v = v
    var equal = a.rows == _v.rows
    equal &&= a.columns == _v.columns
    equal &&= a.a zip _v.a forall { case (a, b) => a == b }
    (equal, "okMessage", _v.toString)
  }
}

case class matchVector[P <: AnyVal, T](a: Vector[P, T]) extends matcher.Matcher[Vector[P, T]] {
  import java.util.Arrays
  def apply(v: => Vector[P, T]) = {
    val _v = v
    var equal = a.length == _v.length
    equal &&= a.v zip _v.v forall { case (a, b) => a == b }
    (equal, "okMessage", _v.toString)
  }
}
