package spire.algebra

import scala.{specialized => spec}

trait CooperativeEq[@spec A, @spec B] extends Any {
  /** Returns `true` if `x` and `y` can be seen as equivalent, `false` otherwise. */
  def eqv(x:A, y:B): Boolean

  /** Returns `false` if `x` and `y` can be seen as equivalent, `true` otherwise. */
  def neqv(x:A, y:B): Boolean = !eqv(x, y)
}

trait CooperativeEqPrio0 extends Any {

  // one of the conversion methods must have a lower prio so the compiler knows which one to choose if both are possible
  implicit def convertLHS[A, B](implicit aToB: A ⇒ B, bEq: Eq[B]): CooperativeEq[A, B] = new CooperativeEq[A, B] {
    def eqv(x: A, y: B) = bEq.eqv(aToB(x), y)
  }
}
trait CooperativeEqPrio1 extends CooperativeEqPrio0 {

  /*
  implicit def flip[A, B](implicit ev: CooperativeEq[B, A]): CooperativeEq[A, B] = new CooperativeEq[A, B] {
    def eqv(x: A, y: B) = ev.eqv(y, x)
  }
  */

  implicit def convertRHS[A, B](implicit aEq: Eq[A], bToA: B ⇒ A): CooperativeEq[A, B] = new CooperativeEq[A, B] {
    def eqv(x: A, y: B) = aEq.eqv(x, bToA(y))
  }
}

object CooperativeEq extends CooperativeEqPrio1 {

  implicit def cooperativeEqFromEq[A, B](implicit ev: Eq[B], s: A =:= B) : CooperativeEq[A, B] = new CooperativeEq[A, B] {
    def eqv(x: A, y: B) = ev.eqv(s.apply(x), y)
  }
}
