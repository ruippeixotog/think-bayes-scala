package thinkbayes.extensions

import org.specs2.mutable.Specification
import thinkbayes._
import thinkbayes.extensions.Stats._

class StatsSpec extends Specification {

  "The Stats extension" should {
    val pmf1 = Pmf(List(15, 20, 35, 40, 50))
    val pmf2 = Pmf(List(3, 6, 7, 8, 8, 10, 13, 15, 16, 20))
    val pmf3 = Pmf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)

    "allow calculating quantiles of a Pmf" in {
      List(0.3, 0.4, 0.5, 1.0).map(pmf1.quantile) === Seq(20, 20, 35, 50)
      (0.0 to 1.0 by 0.25).map(pmf2.quantile) === Seq(3, 7, 8, 15, 20)

      pmf3.quantile(0.1) === 'a'
      pmf3.quantile(0.2) === 'a'
      pmf3.quantile(0.21) === 'b'
      pmf3.quantile(0.4) === 'b'
      pmf3.quantile(0.41) === 'c'
    }

    "allow calculating credible intervals of a Pmf" in {
      todo
    }

    "allow calculating quantiles of a Cdf" in {
      List(0.3, 0.4, 0.5, 1.0).map(pmf1.toCdf.quantile) === Seq(20, 20, 35, 50)
      (0.0 to 1.0 by 0.25).map(pmf2.toCdf.quantile) === Seq(3, 7, 8, 15, 20)

      val cdf3 = pmf3.toCdf
      cdf3.quantile(0.1) === 'a'
      cdf3.quantile(0.2) === 'a'
      cdf3.quantile(0.21) === 'b'
      cdf3.quantile(0.4) === 'b'
      cdf3.quantile(0.41) === 'c'
    }

    "allow calculating credible intervals of a Cdf" in {
      todo
    }
  }
}
