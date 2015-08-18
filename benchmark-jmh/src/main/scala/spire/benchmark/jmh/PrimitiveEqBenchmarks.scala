package spire.benchmark.jmh

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.algebra.Eq
import spire.std.long._

object StrictEqSyntax {

  implicit class EqOps[T](val lhs: T) extends AnyVal {
    // this version of === does not allow implicit conversions to happen because of the =:=
    def =====[U](rhs: U)(implicit ev: Eq[T], u2t: U =:= T) = ev.eqv(lhs, u2t(rhs))
  }
}

object WrapperSyntax {

  implicit class LongWrapper(val x: Long) { lhs => def =+=(rhs: LongWrapper): Boolean = lhs.x == rhs.x }
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

  @Benchmark
  def wrapperEq(x: Blackhole): Unit = {
    import WrapperSyntax._
    x.consume(a =+= b)
  }
}