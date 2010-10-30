package org.no.ip.bca.superlearn.math

class VectorDOps[T](a: Vector[Double, T]) {
  type PrimType = Double
  type MyType = Vector[PrimType, T]

  def +(b: PrimType): MyType => MyType = this.+(b, _)
  def <+>(b: PrimType): MyType = this.+(b, a.empty)
  def +(b: MyType): MyType => MyType = this.+(b, _)
  def <+>(b: MyType): MyType = this.+(b, a.empty)
  def +<(b: PrimType): MyType = this.+(b, a)
  def +<(b: MyType): MyType = this.+(b, a)
  def +>(b: MyType): MyType = this.+(b, b)
  def +(b: PrimType, c: MyType): MyType = {
    assert(a.length == c.length)
    val av = a.v
    val cv = c.v
    var i = 0
    val max = a.length
    while (i < max) {
      cv(i) = av(i) + b
      i += 1
    }
    c
  }
  def +(b: MyType, c: MyType): MyType = {
    assert(a.length == b.length)
    assert(b.length == c.length)
    val av = a.v
    val bv = b.v
    val cv = c.v
    var i = 0
    val max = a.length
    while (i < max) {
      cv(i) = av(i) + bv(i)
      i += 1
    }
    c
  }
  def -(b: PrimType): MyType => MyType = this + -b
  def <->(b: PrimType): MyType = this <+> -b
  def -(b: MyType): MyType => MyType = this.-(b, _)
  def <->(b: MyType): MyType = this.-(b, a.empty)
  def -<(b: PrimType): MyType = this.+(-b, a)
  def -<(b: MyType): MyType = this.-(b, a)
  def ->(b: MyType): MyType = this.-(b, b)
  def -(b: PrimType, c: MyType): MyType = this.+(-b, c)
  def -(b: MyType, c: MyType): MyType = {
    assert(a.length == b.length)
    assert(b.length == c.length)
    val av = a.v
    val bv = b.v
    val cv = c.v
    var i = 0
    val max = a.length
    while (i < max) {
      cv(i) = av(i) - bv(i)
      i += 1
    }
    c
  }
  def *(b: PrimType): MyType => MyType = this.*(b, _)
  def <*>(b: PrimType): MyType = this.*(b, a.empty)
  def *<(b: PrimType): MyType = this.*(b, a)
  def *(b: PrimType, c: MyType) = {
    assert(a.length == c.length)
    val av = a.v
    val cv = c.v
    var i = 0
    val max = a.length
    while (i < max) {
      cv(i) = av(i) * b
      i += 1
    }
    c
  }
  def *[O](b: Matrix[PrimType, T, O]): Vector[PrimType, O] => Vector[PrimType, O] = this.*(b, _)
  def <*>[O](b: Matrix[PrimType, T, O]): Vector[PrimType, O] = this.*(b, Vector.withLength(b.columns))
  def *[O](b: Matrix[PrimType, T, O], c: Vector[PrimType, O]): Vector[PrimType, O] = {
    assert(a ne c)
    assert(a.length == b.rows)
    assert(b.columns == c.length)
    val av = a.v
    val ba = b.a
    val cv = c.v
    var i = 0
    var x = 0
    var y = 0
    var sum = 0.0
    val rows = b.rows
    val columns = b.columns
    val max = b.elementCount
    while (i < max) {
      sum += av(x) * ba(i)
      i += 1
      x += 1
      if (x == rows) {
        cv(y) = sum
        y += 1
        x = 0
        sum = 0.0
      }
    }
    c
  }

  def ^[O](b: Vector[PrimType, O]): Matrix[PrimType, T, O] => Matrix[PrimType, T, O] = this.^(b, _)
  def <^>[O](b: Vector[PrimType, O]): Matrix[PrimType, T, O] = this.^(b, Matrix.withSize(a.length, b.length))
  def ^[O](b: Vector[PrimType, O], c: Matrix[PrimType, T, O]): Matrix[PrimType, T, O] = {
    assert(a.length == c.rows)
    assert(b.length == c.columns)

    val av = a.v
    val bv = b.v
    val ca = c.a
    val columns = c.columns
    val rows = c.rows

    var i = 0
    var j = 0
    var k = 0
    while (j < columns) {
      val bvj = bv(j)
      while (i < rows) {
        ca(k) = av(i) * bvj
        k += 1
        i += 1
      }
      j += 1
      i = 0
    }
    c
  }

  def /(b: PrimType): MyType => MyType = this./(1 / b, _)
  def </>(b: PrimType): MyType = this./(b, a.empty)
  def /<(b: PrimType): MyType = this./(b, a)
  def /(b: PrimType, c: MyType) = {
    assert(a.length == c.length)
    val av = a.v
    val cv = c.v
    var i = 0
    val max = a.length
    while (i < max) {
      cv(i) = av(i) * b
      i += 1
    }
    c
  }

  def |(b: PrimType => PrimType): MyType => MyType = this.|(b, _)
  def <|>(b: PrimType => PrimType): MyType = this.|(b, a.empty)
  def |<(b: PrimType => PrimType): MyType = this.|(b, a)
  def |(b: PrimType => PrimType, c: MyType): MyType = {
    assert(a.length == c.length)
    val av = a.v
    val cv = c.v
    var i = 0
    val max = a.length
    while (i < max) {
      cv(i) = b(av(i))
      i += 1
    }
    c
  }
  def |(b: (Int, PrimType) => PrimType): MyType => MyType = this.|(b, _)
  def <|>(b: (Int, PrimType) => PrimType): MyType = this.|(b, a.empty)
  def |<(b: (Int, PrimType) => PrimType): MyType = this.|(b, a)
  def |(b: (Int, PrimType) => PrimType, c: MyType): MyType = {
    assert(a.length == c.length)
    val av = a.v
    val cv = c.v
    var i = 0
    val max = a.length
    while (i < max) {
      cv(i) = b(i, av(i))
      i += 1
    }
    c
  }
}