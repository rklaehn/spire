package spire.math

import java.math.BigInteger

import spire.algebra.Order

import scala.annotation.tailrec

class Int53 private (private[math] val value: Double) extends AnyVal { lhs ⇒

  import Int53._

  def isZero: Boolean = value == 0D

  def isOne: Boolean = value == Base

  def abs: Int53 = new Int53(value.abs)

  def unary_- : Int53 = new Int53(-value)

  def <(rhs: Int53): Boolean = lhs.value < rhs.value

  def >(rhs: Int53): Boolean = lhs.value > rhs.value

  def <=(rhs: Int53): Boolean = lhs.value <= rhs.value

  def >=(rhs: Int53): Boolean = lhs.value >= rhs.value

  def +(rhs: Int53): Int53 = new Int53(lhs.value + rhs.value)

  def -(rhs: Int53): Int53 = new Int53(lhs.value - rhs.value)

  def *(rhs: Int53): Int53 = new Int53((lhs.value / Base) * rhs.value)

  def %(rhs: Int53): Int53 = {
    if(rhs.isZero)
      throw new ArithmeticException("/ by zero")
    unsafeFromDouble((lhs.value / Base) % (rhs.value / Base))
  }

  def /(rhs: Int53): Int53 = {
    if(rhs.isZero)
      throw new ArithmeticException("/ by zero")
    val fractional = lhs.value / rhs.value
    // we want "integer-like" behavior, but not convert to long since that is slow on JS
    val rounded =
      if(fractional < 0.0)
        fractional.ceil
      else
        fractional.floor
    new Int53(rounded * Base)
  }

  def compare(rhs: Int53) = java.lang.Double.compare(lhs.value, rhs.value)

  def isInfinity: Boolean = value.isInfinity

  def isPosInfinity: Boolean = value.isPosInfinity

  def isNegInfinity: Boolean = value.isNegInfinity

  def toLong: Long =
    if(value.isNaN || value.isInfinity)
      throw new ArithmeticException(s"$this can not be converted to a Long")
    else
      (value / Base).toLong

  def toBigInteger: BigInteger = {
    // todo: bypass long
    BigInteger.valueOf(toLong)
  }

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

  def gcd(x: Int53, y: Int53): Int53 =
    new Int53(doubleGCD(x.toDouble, y.toDouble) * Base)

  private def doubleGCD(x: Double, y: Double): Double = {

    @tailrec
    def gcd0(p: Double, q: Double): Double = {
      if (q == 0)
        p
      else
        gcd0(q, p % q)
    }

    gcd0(x.abs, y.abs)
  }

  private[math] def unsafeFromDouble(value: Double): Int53 =
    if(value.isNaN)
      throw new ArithmeticException
    else
      new Int53(value * Base)

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
