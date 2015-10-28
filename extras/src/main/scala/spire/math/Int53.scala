package spire.math

import spire.algebra.Order

class Int53 private (private[math] val value: Double) extends AnyVal { lhs ⇒

  import Int53._

  def unary_- : Int53 = new Int53(-value)

  def <(rhs: Int53): Boolean = lhs.value < rhs.value

  def >(rhs: Int53): Boolean = lhs.value > rhs.value

  def <=(rhs: Int53): Boolean = lhs.value <= rhs.value

  def >=(rhs: Int53): Boolean = lhs.value >= rhs.value

  def +(rhs: Int53): Int53 = new Int53(lhs.value + rhs.value)

  def -(rhs: Int53): Int53 = new Int53(lhs.value - rhs.value)

  def *(rhs: Int53): Int53 = new Int53((lhs.value / Base) * rhs.value)

  def /(rhs: Int53): Int53 = new Int53((lhs.value / rhs.value).toLong * Base)

  def isInfinity: Boolean = value.isInfinity

  def isPosInfinity: Boolean = value.isPosInfinity

  def isNegInfinity: Boolean = value.isNegInfinity

  def toLong: Long =
    if(value.isNaN || value.isInfinity)
      throw new ArithmeticException(s"$this can not be converted to a Long")
    else
      (value / Base).toLong

  def toDouble: Double =
    value / Base

  override def toString: String =
    if(value.isNaN)
      value.toString
    else if(value.isInfinity)
      value.toString
    else
      (value / Base).toLong.toString
}

object Int53 {

  private def mk(value: Double): Int53 =
    if(value.isNaN)
      throw new ArithmeticException
    else
      new Int53(value)

//  private[math] final val Base = 1.9958403095347198E292
  private[math] final val Base = java.lang.Double.longBitsToDouble(0x7ca0000000000000L)

  def zero: Int53 = new Int53(0.0)

  def one: Int53 = new Int53(Base)

  def MaxValue: Int53 = new Int53(Double.MaxValue)

  def MinValue: Int53 = new Int53(Double.MinValue)

  def apply(value: Int): Int53 = new Int53(value * Base)
}

private[math] trait Int53Order extends Order[Int53] {
  override def eqv(x:Int53, y:Int53): Boolean = x == y
  override def neqv(x:Int53, y:Int53): Boolean = x != y
  override def gt(x: Int53, y: Int53): Boolean = x > y
  override def gteqv(x: Int53, y: Int53): Boolean = x >= y
  override def lt(x: Int53, y: Int53): Boolean = x < y
  override def lteqv(x: Int53, y: Int53): Boolean = x <= y
  def compare(x: Int53, y: Int53): Int = java.lang.Double.compare(x.value, y.value)
}
