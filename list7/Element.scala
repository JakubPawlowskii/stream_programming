import Rational._

object Element {

  private class RationalElement(q: Rational) extends Element {
    def contents = {
      if (q.denom == 1) Array(q.numer.toString)
      else {
        val n      = q.numer.toString
        val d      = q.denom.toString
        val eqSign = "-" * n.length.max(d.length)
        Array(n, eqSign, d)
      }
    }
    override def width = contents(1).length

  }

  private class ArrayElement(
      val contents: Array[String]
  ) extends Element

  private class LineElement(s: String) extends Element {
    val contents        = Array(s)
    override def width  = s.length
    override def height = 1
  }

  private class UniformElement(
      ch: Char,
      override val width: Int,
      override val height: Int
  ) extends Element {
    private val line = ch.toString * width
    def contents     = Array.fill(height)(line)
  }

  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element   = new LineElement(line)
  def elem(q: Rational): Element    = new RationalElement(q)
  def elem(n: Int, d: Int): Element = {
    require(d != 0)
    if(d == 1) elem(n.toString)
    else{
      elem(n.toString) above elem('-', n.toString.length.max(d.toString.length), 1) above elem(d.toString)
    }
  }

}

import Element.elem

abstract class Element {
  def contents: Array[String]
  def width: Int  = contents(0).length
  def height: Int = contents.length

  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem(
      for ((line1, line2) <- this1.contents zip that1.contents)
        yield line1 + line2
    )
  }

  def widen(w: Int): Element =
    if (w <= width) this
    else {
      val left  = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }

  def heighten(h: Int): Element =
    if (h <= height) this
    else {
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot
    }
  def equals(that: Element): Element = {
    this beside elem(" = ") beside that
  }
  def +(that: Element): Element = {
    this beside elem(" + ") beside that
  }
  override def toString = contents mkString "\n"
}
