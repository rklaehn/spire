package spire.math

import org.scalatest.FunSuite
import spire.tests.SpireProperties

import scala.annotation.switch

class Int53Test extends FunSuite {

  val x = Int53.Base
  (x: @switch) match {
    case Int53.Base ⇒ true
    case _ ⇒ false
  }

  test("base") {
    assert(Int53.Base == java.lang.Double.longBitsToDouble(0x7ca0000000000000L))
  }

  test("overflow behavior") {
    assert((Int53.maxValue + Int53.one).isPosInfinity)
    assert((Int53.maxValue * Int53(2)).isPosInfinity)
    assert((Int53.minValue - Int53.one).isNegInfinity)
    assert((Int53.minValue - Int53(2)).isNegInfinity)
  }
}

class Int53Properties extends SpireProperties {

  import org.scalacheck.Arbitrary._

  private def binaryOpTest(a: Int, b: Int, f1: (Long, Long) ⇒ Long, f2: (Int53, Int53) ⇒ Int53): Unit = {
    val r1 = f1(a, b)
    val r2 = f2(Int53(a), Int53(b)).toLong
    assert(r1 == r2)
  }

  property("negation") {
    forAll { x: Int ⇒
      assert(-x.toLong == (-Int53(x)).toLong)
    }
  }

  property("addition") {
    forAll { (x: Int, y: Int) ⇒
      binaryOpTest(x, y, _ + _, _ + _)
    }
  }

  property("subtraction") {
    forAll { (x: Int, y: Int) ⇒
      binaryOpTest(x, y, _ - _, _ - _)
    }
  }

  property("multiplication") {
    forAll { (x: Int, y: Short) ⇒
      binaryOpTest(x, y, _ * _, _ * _)
    }
  }

  property("division") {
    forAll { (x: Int, y: Int) ⇒
      if(y != 0)
        binaryOpTest(x, y, _ / _, _ / _)
    }
  }
}