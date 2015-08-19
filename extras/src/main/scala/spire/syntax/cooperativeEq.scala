package spire.syntax

import language.experimental.macros
import spire.algebra.CooperativeEq

import scala.reflect.macros.whitebox._

final class CooperativeEqOps[A](lhs:A) {
  def =~=[B](rhs:B)(implicit ev:CooperativeEq[A, B]): Boolean = macro CooperativeEqMacros.eqvMacro[A, B]
  def =!=[B](rhs:B)(implicit ev:CooperativeEq[A, B]): Boolean = macro CooperativeEqMacros.neqvMacro[A, B]
}

object CooperativeEqMacros {

  def eqvMacro[A, B](c: Context)(rhs: c.Expr[B])(ev: c.Expr[CooperativeEq[A, B]]): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        if(lhs.tpe.widen == rhs.tree.tpe.widen) {
          val tpe = lhs.tpe.widen
          // this is a very hackish way to get the type spire.algebra.Eq[T], but I could not find another that works.
          // see commented out code for things that did not work
//          val eqTpe = c.typeOf[spire.algebra.Eq[Unit]]
//          val eqTType = eqTpe.substituteTypes(eqTpe.typeParams, List(tpe)).resultType
//          println(eqTType)
//          val tp = c.typeOf[spire.algebra.Eq[_]].typeConstructor
//          println(tp)
          val eqTTpe = c.typecheck(q"null: spire.algebra.Eq[$tpe]").tpe
          val eqT = c.inferImplicitValue(eqTTpe, silent = false)
          q"$eqT.eqv($lhs, $rhs)"
        } else {
          // just use the typeclass instance for now
          q"$ev.eqv($lhs, $rhs)"
        }
      case t =>
        c.abort(c.enclosingPosition, "That was somewhat unexpected!")
    }
  }

  def neqvMacro[A, B](c: Context)(rhs: c.Expr[B])(ev: c.Expr[CooperativeEq[A, B]]): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        if(lhs.tpe.widen == rhs.tree.tpe.widen) {
          val tpe = lhs.tpe.widen
          // this is a very hackish way to get the type spire.algebra.Eq[T], but I could not find another that works.
          // see commented out code for things that did not work
          // val eqTpe = c.typeOf[spire.algebra.Eq[_]].typeConstructor
          // val eqTType = eqTpe.substituteSymbols(eqTpe.typeParams, List(tpe.typeSymbol)).finalResultType
          val eqTTpe = c.typecheck(q"spire.algebra.Eq[$tpe]").tpe
          val eqT = c.inferImplicitValue(eqTTpe, silent = false)
          q"$eqT.neqv($lhs, $rhs)"
        } else {
          // just use the typeclass instance for now
          q"$ev.neqv($lhs, $rhs)"
        }
      case t =>
        c.abort(c.enclosingPosition, "That was somewhat unexpected!")
    }
  }
}

trait CooperativeEqSyntax {
  implicit def cooperativeEqOps[A](a:A): CooperativeEqOps[A] = new CooperativeEqOps(a)
}

object cooperativeEq extends CooperativeEqSyntax
