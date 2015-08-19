package spire.benchmark.jmh

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.algebra.Eq
import spire.algebra.CooperativeEq
import spire.std.long._

object StrictEqSyntax {

  implicit class EqOps[T](val lhs: T) extends AnyVal {
    // this version of === does not allow implicit conversions to happen because of the =:=
    def =====[U](rhs: U)(implicit ev: Eq[T], u2t: U =:= T) = ev.eqv(lhs, u2t(rhs))
  }
}

case class Foo(x: Int)
case class Bar(x: Int)
object FooBarEq {

  implicit val cooperativeEq: CooperativeEq[Foo, Bar] = new CooperativeEq[Foo, Bar] {

    def eqv(a: Foo, b: Bar): Boolean = a.x == b.x
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class LongCooperativeEqualityCheckBenchmark {
  import FooBarEq._

  var a: Long = 0L

  var b: Long = 0L

  var c: Int = 0

  var d: Foo = new Foo(0)

  var e: Bar = new Bar(0)

  @Benchmark
  def eqtildeeqLongLong(x: Blackhole): Unit = {
    import spire.syntax.cooperativeEq._
    x.consume(a =~= b)
  }

  @Benchmark
  def eqtildeeqLongInt(x: Blackhole): Unit = {
    import spire.syntax.cooperativeEq._
    x.consume(a =~= c)
  }

  @Benchmark
  def eqtildeeqIntLong(x: Blackhole): Unit = {
    import spire.syntax.cooperativeEq._
    x.consume(c =~= b)
  }

  @Benchmark
  def eqtildeeqFooBar(x: Blackhole): Unit = {
    import spire.syntax.cooperativeEq._
    x.consume(d =~= e)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class LongEqualityCheckBenchmark {

  var a: Long = 0L

  var b: Long = 0L

  @Benchmark
  def compare(x: Blackhole): Unit = {
    x.consume(a compare b)
  }

  @Benchmark
  def eqeq(x: Blackhole): Unit = {
    x.consume(a == b)
  }

  @Benchmark
  def eqeqeq(x: Blackhole): Unit = {
    import spire.implicits._
    x.consume(a === b)
  }

  @Benchmark
  def strictEq(x: Blackhole): Unit = {
    import StrictEqSyntax._
    x.consume(a ===== b)
  }

  @Benchmark
  def strictEqMacro(x: Blackhole): Unit = {
    import spire.syntax.strictEq._
    x.consume(a ==== b)
  }
}