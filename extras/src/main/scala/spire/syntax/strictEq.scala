package spire.syntax
import spire.algebra.Eq

import scala.reflect.macros.whitebox.Context

object strictEq {

  implicit class StrictEqOps[A](val lhs: A)(implicit ev: Eq[A]) {

    def ====[B](rhs: B)(implicit same: A =:= B): Boolean = macro strictEqMacro[A, B]
  }

  def strictEqMacro[A, B](c: Context)(rhs: c.Expr[B])(same: c.Expr[A =:= B]): c.Tree = {
    import c.universe._
    val Apply(Apply(TypeApply(_, _), List(lhs)), List(ev)) = c.prefix.tree
    q"$ev.eqv($lhs, $rhs)"
  }
}
