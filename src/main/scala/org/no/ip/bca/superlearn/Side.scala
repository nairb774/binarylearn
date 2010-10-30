package org.no.ip.bca.superlearn

sealed class Side
final object Side {
  final case class H extends Side
  final case class V extends Side
}