package org.no.ip.bca.superlearn

import org.specs._

case class matchDoubleArray(a: Array[Double]) extends matcher.Matcher[Array[Double]] {
  import java.util.Arrays
  def apply(v: => Array[Double]) = {
    val _v = v
    (Arrays.equals(_v, a), "okMessage", Arrays.toString(_v))
  }
}

case class matchLongArray(a: Array[Long]) extends matcher.Matcher[Array[Long]] {
  import java.util.Arrays
  def apply(v: => Array[Long]) = {
    val _v = v
    (Arrays.equals(_v, a), "okMessage", Arrays.toString(_v))
  }
}