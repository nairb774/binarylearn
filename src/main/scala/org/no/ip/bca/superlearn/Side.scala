package org.no.ip.bca.superlearn

sealed class Side
final object Side {
  sealed trait H extends Side
  sealed trait V extends Side
}