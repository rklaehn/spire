package spire.syntax

import language.experimental.macros
import spire.algebra.CooperativeEq

final class CooperativeEqOps[A](lhs:A) {
  def =~=[B](rhs:B)(implicit ev:CooperativeEq[A, B]): Boolean = ev.eqv(lhs, rhs)
  def =!=[B](rhs:B)(implicit ev:CooperativeEq[A, B]): Boolean = ev.neqv(lhs, rhs)
}

trait CooperativeEqSyntax {
  implicit def cooperativeEqOps[A](a:A): CooperativeEqOps[A] = new CooperativeEqOps(a)
}

object cooperativeEq extends CooperativeEqSyntax
