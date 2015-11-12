package spire.math
import java.math._

sealed abstract class SafeInt { lhs ⇒
  import SafeInt._

  final def signum: Int = this match {
    case Small(x) ⇒ x.value.signum
    case Large(x) ⇒ x.signum
  }

  final def compare(rhs: SafeInt) = (lhs, rhs) match {
    case (Small(lhs), Small(rhs)) ⇒ lhs compare rhs
    case (Small(lhs), Large(rhs)) ⇒ -rhs.signum
    case (Large(lhs), Small(rhs)) ⇒ lhs.signum
    case (Large(lhs), Large(rhs)) ⇒ lhs compareTo rhs
  }

  def +(rhs: SafeInt): SafeInt = choose(this.toBigInteger add rhs.toBigInteger)

  def -(rhs: SafeInt): SafeInt = choose(this.toBigInteger subtract rhs.toBigInteger)

  def *(rhs: SafeInt): SafeInt = choose(this.toBigInteger multiply rhs.toBigInteger)

  def /(rhs: SafeInt): SafeInt = choose(this.toBigInteger divide rhs.toBigInteger)

  def %(rhs: SafeInt): SafeInt = choose(this.toBigInteger mod rhs.toBigInteger)

  def /%(rhs: SafeInt): (SafeInt, SafeInt) = {
    val Array(q, r) = lhs.toBigInteger divideAndRemainder rhs.toBigInteger
    (choose(q), choose(r))
  }

  def &(rhs: SafeInt): SafeInt = choose(lhs.toBigInteger and rhs.toBigInteger)

  def |(rhs: SafeInt): SafeInt = choose(lhs.toBigInteger or rhs.toBigInteger)

  def ^(rhs: SafeInt): SafeInt = choose(lhs.toBigInteger xor rhs.toBigInteger)

  def unary_- : SafeInt = this match {
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

object SafeInt {

  val zero = apply(0)

  val one = apply(1)

  def apply(value: Int): SafeInt = Small(Int53(value))

  def apply(value: Long): SafeInt = choose(BigInteger.valueOf(value))

  def apply(value: BigInteger): SafeInt = choose(value)

  def gcd(a: SafeInt, b: SafeInt): SafeInt = (a, b) match {
    case (Small(a), Small(b)) ⇒ Small(Int53.gcd(a, b))
    case _ ⇒ choose(a.toBigInteger gcd b.toBigInteger)
  }

  private case class Small(value: Int53) extends SafeInt {

    override def +(rhs: SafeInt): SafeInt = rhs match {
      case Small(rvalue) ⇒
        val result = value + rvalue
        if(!result.isInfinity)
          Small(result)
        else
          Large(value.toBigInteger add rvalue.toBigInteger)
      case _ ⇒ super.+(rhs)
    }

    override def -(rhs: SafeInt): SafeInt = rhs match {
      case Small(rvalue) ⇒
        val result = value - rvalue
        if(!result.isInfinity)
          Small(result)
        else
          Large(value.toBigInteger subtract rvalue.toBigInteger)
      case _ ⇒ super.-(rhs)
    }

    override def *(rhs: SafeInt): SafeInt = rhs match {
      case Small(rvalue) ⇒
        val result = value * rvalue
        if(!result.isInfinity)
          Small(result)
        else
          Large(value.toBigInteger multiply rvalue.toBigInteger)
      case _ ⇒ super.*(rhs)
    }

    override def /(rhs: SafeInt): SafeInt = rhs match {
      case Small(rvalue) ⇒ Small(value / rvalue)
      case _ ⇒ super./(rhs)
    }

    override def %(rhs: SafeInt): SafeInt = rhs match {
      case Small(rvalue) ⇒ Small(value % rvalue)
      case _ ⇒ super.%(rhs)
    }

    override def /%(that: SafeInt): (SafeInt, SafeInt) = (this / that, this % that)
  }

  private case class Large(value: BigInteger) extends SafeInt

  private val int53MinValueMinusOne = java.math.BigInteger.valueOf(Int53.MinValue.toLong - 1)

  private def choose(value: BigInteger): SafeInt = {
    val bits = value.bitLength
    val isSmall = (bits < 53) || ((bits == 53) && (value != int53MinValueMinusOne))
    if(isSmall)
      Small(Int53.unsafeFromDouble(value.doubleValue))
    else
      Large(value)
  }
}
