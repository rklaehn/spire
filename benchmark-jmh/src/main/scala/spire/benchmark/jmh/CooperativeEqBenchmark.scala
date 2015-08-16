package spire.benchmark.jmh

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.math.Rational
import spire.syntax.cooperativeEq._
import spire.syntax.eq._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class CooperativeEqBenchmarkRational {

  var a: Rational = Rational(1234)

  var b: Rational = Rational(1234)

  @Setup
  def setup(): Unit = {
  }

  @Benchmark
  def equals(x: Blackhole): Unit = {
    x.consume(a equals b)
  }

  @Benchmark
  def eqeq(x: Blackhole): Unit = {
    x.consume(a == b)
  }

  @Benchmark
  def eqeqInt(x: Blackhole): Unit = {
    x.consume(a == 1234)
  }

  @Benchmark
  def eqeqeq(x: Blackhole): Unit = {
    x.consume(a === b)
  }

  @Benchmark
  def cooperativeEq(x: Blackhole): Unit = {
    x.consume(a =~= b)
  }

  @Benchmark
  def cooperativeEqInt(x: Blackhole): Unit = {
    x.consume(a =~= 1234)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class CooperativeEqBenchmarkString {

  var a: String = "1234"

  var b: String = "1235"

  @Setup
  def setup(): Unit = {
  }

  @Benchmark
  def equal(x: Blackhole): Unit = {
    x.consume(a equals b)
  }

  @Benchmark
  def eqeq(x: Blackhole): Unit = {
    x.consume(a == b)
  }
}