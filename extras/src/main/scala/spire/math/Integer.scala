package spire.math
import java.math._

sealed abstract class Integer { lhs ⇒
  import Integer._

  final def +(rhs: Integer): Integer = (lhs, rhs) match {
    case (Small(lhs), Small(rhs)) ⇒
      val result = lhs + rhs
      if(!result.isInfinity)
        Small(result)
      else
        Large(lhs.toBigInteger add rhs.toBigInteger)
    case _ ⇒
      choose(lhs.toBigInteger add rhs.toBigInteger)
  }

  def -(rhs: Integer): Integer = (lhs, rhs) match {
    case (Small(lhs), Small(rhs)) ⇒
      val result = lhs - rhs
      if(!result.isInfinity)
        Small(result)
      else
        Large(lhs.toBigInteger subtract rhs.toBigInteger)
    case _ ⇒
      choose(lhs.toBigInteger subtract rhs.toBigInteger)
  }

  def *(rhs: Integer): Integer = (lhs, rhs) match {
    case (Small(lhs), Small(rhs)) ⇒
      val result = lhs * rhs
      if(!result.isInfinity)
        Small(result)
      else
        Large(lhs.toBigInteger multiply rhs.toBigInteger)
    case _ ⇒
      choose(lhs.toBigInteger multiply rhs.toBigInteger)
  }

  def /(rhs: Integer): Integer = (lhs, rhs) match {
    case (Small(lhs), Small(rhs)) ⇒
      val result = lhs / rhs
      if(!result.isInfinity)
        Small(result)
      else
        Large(lhs.toBigInteger divide rhs.toBigInteger)
    case _ ⇒
      choose(lhs.toBigInteger divide rhs.toBigInteger)
  }

  def unary_- : Integer = this match {
    case Small(value) ⇒ Small(-value)
    case Large(value) ⇒ Large(value.negate)
  }

  def toBigInteger: BigInteger = this match {
    case Small(value) ⇒ value.toBigInteger
    case Large(value) ⇒ value
  }

  override def toString: String = this match {
    case Small(value) ⇒ value.toString
    case Large(value) ⇒ value.toString
  }

  override def hashCode: Int = this match {
    case Small(value) ⇒ value.hashCode
    case Large(value) ⇒ value.hashCode
  }
}

object Integer {

  val zero = apply(0)

  val one = apply(1)

  def apply(value: Int): Integer = Small(Int53(value))

  private case class Small(value: Int53) extends Integer

  private case class Large(value: BigInteger) extends Integer

  private val int53MinValueMinusOne = java.math.BigInteger.valueOf(Int53.MinValue.toLong - 1)

  private def choose(value: BigInteger): Integer = {
    val bits = value.bitLength
    val isSmall = (bits < 53) || ((bits == 53) && (value != int53MinValueMinusOne))
    if(isSmall)
      Small(Int53.unsafeFromDouble(value.doubleValue))
    else
      Large(value)
  }
}
