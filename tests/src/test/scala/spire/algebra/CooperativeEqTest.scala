package spire.algebra

import org.scalatest.FunSuite
import spire.syntax.cooperativeEq._
import spire.implicits._

class CooperativeEqTest extends FunSuite {

  case class Foo(x: Int)

  case class Bar(x: Int)

  case class Baz(x: Int)

  object Bar {
    implicit def fromFoo(foo: Foo): Bar = Bar(foo.x)

    implicit val eq = Eq.by[Bar, Int](_.x)
  }

  // explicitly given cooperative equality for Foo and Baz. Will also work for Baz and Foo due to swap
  implicit object FooBazEq extends CooperativeEq[Foo, Baz] {
    override def eqv(a: Foo, b: Baz): Boolean = a.x == b.x
  }

  test("simple") {
    assert(1 =~= 1)
    assert(1L =~= 1)
    assert(1 =~= 1L)
    assert(Foo(1) =~= Bar(1))
    assert(Bar(1) =~= Foo(1))
    assert(Foo(1) =~= Baz(1))
//    assert(Baz(1) =~= Foo(1))
  }
  test("reify") {
    import scala.reflect.runtime.{universe => u}
    assert(u.reify { 1 =~= 1 }.toString.contains("cooperativeEqFromEq"))
    assert(u.reify { 1L =~= 1 }.toString.contains("convertRHS"))
    assert(u.reify { 1 =~= 1L }.toString.contains("convertLHS"))
    assert(u.reify { Foo(1) =~= Bar(1) }.toString.contains("convertLHS"))
    assert(u.reify { Bar(1) =~= Foo(1) }.toString.contains("convertRHS"))
    assert(u.reify { Foo(1) =~= Baz(1) }.toString.contains("FooBazEq"))
    // println(u.reify { 1 =~= "x" }.toString.contains("could not find implicit"))
  }
}
