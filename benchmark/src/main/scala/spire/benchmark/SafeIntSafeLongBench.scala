package spire.benchmark

import spire.math.{SafeInt, SafeLong}

object SafeIntSafeLongBench {

  def sum1(n: Int): Any = {
    var t: SafeLong = SafeLong.zero
    var i = 0
    while(i < n) {
      t += SafeLong(i)
      i += 1
    }
    t
  }

  def sum2(n: Int): Any = {
    var t: SafeInt = SafeInt.zero
    var i = 0
    while(i < n) {
      t += SafeInt(i)
      i += 1
    }
    t
  }

  def product1(n: Int): Any = {
    var t: SafeLong = SafeLong.one
    var i = 2
    while(i <= n) {
      t *= SafeLong(i)
      i += 1
    }
    t
  }

  def product2(n: Int): Any = {
    var t: SafeInt = SafeInt.one
    var i = 2
    while(i <= n) {
      t *= SafeInt(i)
      i += 1
    }
    t
  }

  def main(args: Array[String]): Unit = {
    val th = ichi.bench.Thyme.warmed(verbose = println)
    th.pbenchOffWarm("sum SafeLong vs. SafeInt")(th.Warm(sum1(1000000)))(th.Warm(sum2(1000000)))
    th.pbenchOffWarm("product SafeLong vs. SafeInt")(th.Warm(product1(100)))(th.Warm(product2(100)))
  }
}
