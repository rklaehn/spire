package spire.math

import compat._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

/**
 * Polynomial
 * A univariate polynomial class and EuclideanRing extension trait 
 * for arithmetic operations.Polynomials can be instantiated using 
 * any type C, with exponents given by Long values. Arithmetic and 
 * many other basic operations require either implicit Ring[C] 
 * and/or Field[C]'s in scope.
*/


// Univariate polynomial term
case class Term[C](coeff: C, exp: Long) {

  def toTuple: (Long, C) = (exp, coeff)

  def eval(x: C)(implicit r: Ring[C]): C =
    coeff * (x pow exp.intValue)

  def isIndexZero: Boolean = 
    exp == 0L

  def isZero(implicit eq: Eq[C], r: Ring[C]): Boolean =
    coeff === r.zero

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der(implicit r: Ring[C]): Term[C] =
    Term(coeff * r.fromInt(exp.intValue), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt((exp + 1).intValue), exp + 1)

  def termString(implicit o: Order[C], r: Ring[C]) = {
    import r._
    (coeff, exp.intValue) match {
      case (0, _) => ""
      case (c, 0) => if (c >= zero) s" + ${c}" else s" - ${-c}"
      case (1, e) => if (e > 1) s" + x^$e" else s" + x"
      case (-1, e) => if (e > 1) s" - x^$e" else s" - x"
      case (c, 1) => if (c >= zero) s" + ${c}x" else s" - ${-c}x"
      case (c, e) => if (c >= zero) s" + ${c}x^$e" else s" - ${-c}x^$e"
    }
  }
}

object Term {
  def fromTuple[C](tpl: (Long, C)): Term[C] = Term(tpl._2, tpl._1)
  def zero[C](implicit r: Ring[C]): Term[C] = Term(r.zero, 0L)
  def one[C](implicit r: Ring[C]): Term[C] = Term(r.one, 0L)
}


// Univariate polynomial class
class Polynomial[C: ClassTag] private[spire] (val data: Map[Long, C]) {

  override def toString: String =
    "Polynomial(%s)" format data

  override def equals(that: Any): Boolean = that match {
    case p: Polynomial[_] => data == p.data
    case _ => false
  }

  def terms: Array[Term[C]] =
    data.map(Term.fromTuple).toArray

  implicit object BigEndianPolynomialOrdering extends Order[Term[C]] {
    def compare(x:Term[C], y:Term[C]): Int = y.exp compare x.exp
  }

  def allTerms(implicit r: Ring[C]): Array[Term[C]] = {
    val m = maxOrder
    val cs = new Array[Term[C]]((m + 1).intValue)
    terms.foreach(t => cs(t.exp.intValue) = t)
    for(i <- 0 to m.intValue)
      if (cs(i) == null) cs(i) = Term(r.zero, i)
    cs.reverse
  }

  def coeffs(implicit r: Ring[C]): Array[C] =
    allTerms.map(_.coeff)

  def maxTerm(implicit r: Ring[C]): Term[C] =
    data.foldLeft(Term.zero[C]) { case (term, (e, c)) =>
      if (term.exp <= e) Term(c, e) else term
    }

  def maxOrder(implicit r: Ring[C]): Long =
    if (data.isEmpty) 0 else data.keys.qmax

  def maxOrderTermCoeff(implicit r: Ring[C]): C =
    maxTerm.coeff

  def degree(implicit r: Ring[C], eq: Eq[C]): Long =
    data.foldLeft(0) { case (d, (e, c)) =>
      if (e > d && c =!= r.zero) e.intValue else d
    }

  def apply(x: C)(implicit r: Ring[C]): C =
    data.foldLeft(r.zero)((sum, t) => sum + Term.fromTuple(t).eval(x))

  def isZero(implicit r: Ring[C], eq: Eq[C]): Boolean =
    data.forall { case (e, c) => c === r.zero }

  def monic(implicit f: Field[C]): Polynomial[C] = {
    val m = maxOrderTermCoeff
    new Polynomial(data.map { case (e, c) => (e, c / m) })
  }

  def derivative(implicit r: Ring[C], eq: Eq[C]): Polynomial[C] =
    Polynomial(data.flatMap { case (e, c) =>
      if (e > 0) Some(Term(c, e).der) else None
    })

  def integral(implicit f: Field[C], eq: Eq[C]): Polynomial[C] =
    Polynomial(data.map(t => Term.fromTuple(t).int))

  def show(implicit o: Order[C], r: Ring[C]) : String =
    if (isZero) {
      "(0)"
    } else {
      val ts = terms
      QuickSort.sort(ts)
      val s = ts.map(_.termString).mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}


object Polynomial {

  def apply[C: ClassTag](data: Map[Long, C])(implicit eq: Eq[C], r: Ring[C]): Polynomial[C] =
    new Polynomial(data.filter { case (e, c) => c =!= r.zero })

  /* We have to get rid of coeff=zero terms here for long division
   * operations.
   * I think we should have an Eq[C] and Ring[C] requirement for Polys.
   */
  def apply[C: ClassTag](terms: Iterable[Term[C]])(implicit eq: Eq[C], r: Ring[C]): Polynomial[C] =
    new Polynomial(terms.foldLeft(Map.empty[Long, C]) { case (m, Term(c, e)) =>
      if (c === r.zero) m else m.updated(e, m.get(e).map(_ + c).getOrElse(c))
    })

  def apply[C: ClassTag](c: C, e: Long): Polynomial[C] =
    new Polynomial(Map(e -> c))

  private val termRe = "([0-9]+\\.[0-9]+|[0-9]+/[0-9]+|[0-9]+)?(?:([a-z])(?:\\^([0-9]+))?)?".r
  private val operRe = " *([+-]) *".r

  def apply(s: String): Polynomial[Rational] = {

    // represents a term, plus a named variable v
    case class T(c: Rational, v: String, e: Long)

    // parse all the terms and operators out of the string
    @tailrec def parse(s: String, ts: List[T]): List[T] =
      if (s.isEmpty) {
        ts
      } else {
        val (op, s2) = operRe.findPrefixMatchOf(s) match {
          case Some(m) => (m.group(1), s.substring(m.end))
          case None => if (ts.isEmpty) ("+", s) else sys.error(s"parse error: $s")
        }

        val m2 = termRe.findPrefixMatchOf(s2).getOrElse(sys.error("parse error: $s2"))
        val c0 = Option(m2.group(1)).getOrElse("1")
        val c = if (op == "-") "-" + c0 else c0
        val v = Option(m2.group(2)).getOrElse("")
        val e0 = Option(m2.group(3)).getOrElse("")
        val e = if (e0 != "") e0 else if (v == "") "0" else "1"

        val t = try {
          T(Rational(c), v, e.toLong)
        } catch {
          case _: Exception => sys.error(s"parse error: $c $e")
        }
        parse(s2.substring(m2.end), if (t.c == 0) ts else t :: ts)
      }

    // do some pre-processing to remove whitespace/outer parens
    val t = s.trim
    val u = if (t.startsWith("(") && t.endsWith(")")) t.substring(1, t.length - 1) else t

    // parse out the terms
    val ts = parse(u, Nil)

    // make sure we have at most one variable
    val vs = ts.map(_.v).toSet.filter(_ != "")
    if (vs.size > 1) sys.error("only univariate polynomials supported")

    // we're done!
    Polynomial(ts.map(t => (t.e, t.c)).toMap)
  }

  implicit def pRD: PolynomialRing[Double] = new PolynomialRing[Double] {
    val ct = classTag[Double]
    val r = Ring[Double]
    val o = Order[Double]
    val f = Field[Double]
  }

  implicit def pRR: PolynomialRing[Rational] = new PolynomialRing[Rational] {
    val ct = classTag[Rational]
    val r = Ring[Rational]
    val o = Order[Rational]
    val f = Field[Rational]
  }
}



// Univariate Polynomials Form a EuclideanRing
trait PolynomialRing[C] extends EuclideanRing[Polynomial[C]] {

  implicit def ct: ClassTag[C]
  implicit def r: Ring[C]
  implicit def o: Order[C]
  implicit def f: Field[C]

  implicit def tR: Ring[Term[C]] = new Ring[Term[C]] {
    def negate(t: Term[C]): Term[C] = Term(-t.coeff, t.exp)
    def zero: Term[C] = Term(r.zero, 0L)
    def one: Term[C] = Term(r.one, 0L)
    def plus(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff + y.coeff, y.exp)
    def times(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff * y.coeff, x.exp + y.exp)
  }

  def zero = Polynomial(Map(0L -> r.zero))

  def one = Polynomial(Map(0L -> r.one))

  def negate(x: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data.map { case (e, c) => (e, -c) })

  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data + y.data)

  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data.foldLeft(Map.empty[Long, C]) { case (m, (ex, cx)) =>
      y.data.foldLeft(m) { case (m, (ey, cy)) =>
        val e = ex + ey
        val c = cx * cy
        m.updated(e, m.get(e).map(_ + c).getOrElse(c))
      }
    })

  def quotMod(x: Polynomial[C], y: Polynomial[C]): (Polynomial[C], Polynomial[C]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")
      
    def zipSum(x: Array[C], y: Array[C]): Polynomial[C] = {
      val (s, l) = if(x.length > y.length) (y, x) else (x, y)
      val cs = s.zip(l).map(z => z._1 + z._2) ++ l.drop(s.length)
      Polynomial(cs.zip(((cs.length - 1) to 0 by -1)).tail.map {
        case (c, e) => Term(c, e)
      })
    }

    def polyFromCoeffsLE(cs: Iterable[C]): Polynomial[C] =
      Polynomial(cs.zipWithIndex.map { case (c, e) => Term(c, e) })
      
    @tailrec def eval(q: List[C], u: Polynomial[C], n: Long): (Polynomial[C], Polynomial[C]) = {
      lazy val q0 = u.maxOrderTermCoeff / y.maxOrderTermCoeff
      lazy val uprime = zipSum(u.coeffs, y.coeffs.map(_ * -q0))
      if (u.isZero || n < 0) (polyFromCoeffsLE(q), u) else eval(q0 :: q, uprime, n - 1)
    }

    val ym = y.maxTerm
    if (ym.exp == 0L) {
      val q = Polynomial(x.data.map { case (e, c) => (e, c / ym.coeff) })
      val r = Polynomial(Map.empty[Long, C])
      (q, r)
    } else {
      eval(Nil, x, x.degree - y.degree)
    }
  }

  def quot(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = quotMod(x, y)._1
    
  def mod(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = quotMod(x, y)._2

  @tailrec final def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    if (y.isZero && x.isZero) zero
    else if (y.maxTerm.isZero) x
    else gcd(y, mod(x, y))

}
