package spire.math

import java.math.BigInteger

import org.scalatest.FunSuite
import spire.tests.SpireProperties

class SafeIntTest extends FunSuite {

  def isSmall(value: SafeInt) = value.getClass.getName.endsWith("Small")

  test("switchSmallLarge") {
    assert(isSmall(SafeInt(Int53.MaxValue.toLong)))
    assert(!isSmall(SafeInt(Int53.MaxValue.toLong + 1)))
    assert(isSmall(SafeInt(Int53.MaxValue.toLong + 1) - SafeInt.one))

    assert(isSmall(SafeInt(Int53.MinValue.toLong)))
    assert(!isSmall(SafeInt(Int53.MinValue.toLong - 1)))
    assert(isSmall(SafeInt(Int53.MinValue.toLong - 1) + SafeInt.one))
  }
}

class SafeIntProperties extends SpireProperties {

  import spire.laws.arb.bigInteger

  private val maxSmall = Int53.MaxValue.toBigInteger

  private val minSmall = Int53.MinValue.toBigInteger

  def isSmall(value: SafeInt) = value.getClass.getName.endsWith("Small")

  private def checkInvariants(x: SafeInt): Unit = {
    val xi = x.toBigInteger
    val xIsSmall = !(xi.compareTo(maxSmall) > 0 || xi.compareTo(minSmall) < 0)
    assert(xIsSmall == isSmall(x))
  }

  private def binaryOpTest(a: BigInteger, b: BigInteger, f1: (SafeInt, SafeInt) ⇒ SafeInt, f2: (BigInteger, BigInteger) ⇒ BigInteger): Unit = {
    val t = f1(SafeInt(a), SafeInt(b))
    checkInvariants(t)
    val r1 = t.toBigInteger
    val r2 = f2(a, b)
    assert(r1 == r2)
  }

  private def binaryBooleanOpTest[T](a: BigInteger, b: BigInteger, f1: (SafeInt, SafeInt) ⇒ T, f2: (BigInteger, BigInteger) ⇒ T): Unit = {
    val r1 = f1(SafeInt(a), SafeInt(b))
    val r2 = f2(a, b)
    assert(r1 == r2)
  }

  property("negation") {
    forAll { x: BigInteger ⇒
      assert(x.negate == (-SafeInt(x)).toBigInteger)
    }
  }

  property("addition") {
    forAll { (x: BigInteger, y: BigInteger) ⇒
      binaryOpTest(x, y, _ + _, _ add _)
    }
  }

  property("subtraction") {
    forAll { (x: BigInteger, y: BigInteger) ⇒
      binaryOpTest(x, y, _ - _, _ subtract _)
    }
  }

  property("multiplication") {
    forAll { (x: BigInteger, y: BigInteger) ⇒
      binaryOpTest(x, y, _ * _, _ multiply _)
    }
  }

  property("division") {
    forAll { (x: BigInteger, y: BigInteger) ⇒
      if(y.signum != 0)
        binaryOpTest(x, y, _ / _, _ divide _)
    }
  }

  property("mod") {
    forAll { (x: BigInteger, y: BigInteger) ⇒
      if(x.signum > 0 && y.signum > 0)
        binaryOpTest(x, y, _ % _, _ mod _)
    }
  }

  property("gcd") {
    forAll { (x: BigInteger, y: BigInteger) ⇒
      binaryOpTest(x, y, SafeInt.gcd, _ gcd _)
    }
  }

  property("compare") {
    forAll { (x: BigInteger, y: BigInteger) ⇒
      binaryBooleanOpTest(x, y, _ compare _, _ compareTo _)
    }
  }
}
