package org.no.ip.bca.superlearn.math

class MatrixDOps[L, R](a: Matrix[Double, L, R]) {
  type PrimType = Double
  type MyType = Matrix[PrimType, L, R]
  
  def +(b: MyType): MyType => MyType = this.+(b, _)
  def <+>(b: MyType): MyType = this.+(b, a.empty)
  def +<(b: MyType): MyType = this.+(b, a)
  def +>(b: MyType): MyType = this.+(b, b)
  def +(b: MyType, c: MyType): MyType = {
    assert(a.rows == b.rows)
    assert(b.rows == c.rows)
    assert(a.columns == b.columns)
    assert(b.columns == c.columns)

    val aa = a.a
    val ba = b.a
    val ca = c.a
    var i = 0
    val max = a.elementCount
    while (i < max) {
      ca(i) = aa(i) + ba(i)
      i += 1
    }
    c
  }
  def -(b: MyType): MyType => MyType = this.-(b, _)
  def <->(b: MyType): MyType = this.-(b, a.empty)
  def -<(b: MyType): MyType = this.-(b, a)
  def ->(b: MyType): MyType = this.-(b, b)
  def -(b: MyType, c: MyType): MyType = {
    assert(a.rows == b.rows)
    assert(b.rows == c.rows)
    assert(a.columns == b.columns)
    assert(b.columns == c.columns)

    val aa = a.a
    val ba = b.a
    val ca = c.a
    var i = 0
    val max = a.elementCount
    while (i < max) {
      ca(i) = aa(i) - ba(i)
      i += 1
    }
    c
  }
  
  def *(b: PrimType): MyType => MyType = this.*(b, _)
  def <*>(b: PrimType): MyType = this.*(b, a.empty)
  def *<(b: PrimType): MyType = this.*(b, a)
  def *(b: PrimType, c: MyType): MyType = {
    assert(a.rows == c.rows)
    assert(a.columns == c.columns)

    val aa = a.a
    val ca = c.a
    var i = 0
    val max = a.elementCount
    while (i < max) {
      ca(i) = aa(i) * b
      i += 1
    }
    c
  }
  
  def /(b: PrimType): MyType => MyType = this./(b, _)
  def </>(b: PrimType): MyType = this./(b, a.empty)
  def /<(b: PrimType): MyType = this./(b, a)
  def /(b: PrimType, c: MyType): MyType = {
    assert(a.rows == c.rows)
    assert(a.columns == c.columns)

    val aa = a.a
    val ca = c.a
    var i = 0
    val max = a.elementCount
    while (i < max) {
      ca(i) = aa(i) / b
      i += 1
    }
    c
  }

  def |(b: PrimType => PrimType): MyType => MyType = this.|(b, _)
  def <|>(b: PrimType => PrimType): MyType = this.|(b, a.empty)
  def |<(b: PrimType => PrimType): MyType = this.|(b, a)
  def |(b: PrimType => PrimType, c: MyType): MyType = {
    assert(a.rows == c.rows)
    assert(a.columns == c.columns)
    val aa = a.a
    val ca = c.a
    var i = 0
    val max = a.elementCount
    while (i < max) {
      ca(i) = b(aa(i))
      i += 1
    }
    c
  }
  
  def |(b: (Int, Int, PrimType) => PrimType): MyType => MyType = this.|(b, _)
  def <|>(b: (Int, Int, PrimType) => PrimType): MyType = this.|(b, a.empty)
  def |<(b: (Int, Int, PrimType) => PrimType): MyType = this.|(b, a)
  def |(b: (Int, Int, PrimType) => PrimType, c: MyType): MyType = {
    assert(a.rows == c.rows)
    assert(a.columns == c.columns)
    val aa = a.a
    val ca = c.a
    val rows = a.rows
    
    var i = 0
    var row = 0
    var column = 0
    val max = a.elementCount
    while (i < max) {
      ca(i) = b(row, column, aa(i))
      i += 1
      row += 1
      if (row == rows) {
        row = 0
        column += 1
      }
    }
    c
  }
}