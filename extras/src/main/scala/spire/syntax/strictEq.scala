package spire.syntax
import spire.algebra.Eq

import scala.reflect.macros.whitebox.Context

object strictEq {

  implicit class StrictEqOps[A](val lhs: A) extends AnyVal {

    def ====[B](rhs: B)(implicit ev: Eq[A], same: A =:= B): Boolean = macro strictEqMacro[A, B]
  }

  def strictEqMacro[A, B](c: Context)(rhs: c.Expr[B])(ev: c.Expr[Eq[A]], same: c.Expr[A =:= B]): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        q"$ev.eqv($lhs, $rhs)"
      case t =>
        c.abort(c.enclosingPosition, "That was somewhat unexpected!")
    }
  }
}
