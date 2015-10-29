package thinkbayes.extensions.distributions

import org.apache.commons.math3.util.FastMath._
import org.specs2.mutable.Specification
import SpecialFunctions._

class SpecialFunctionsSpec extends Specification {
  val epsilon = 0.0001

  def beRelativelyCloseTo(expected: Double) = beCloseTo(expected, abs(expected) * epsilon)

  "SpecialFunctions" should {

    "contain correct gamma and logGamma implementations" in {
      val table = Map(
        -19.000001 -> 8.220610833201116e-12,
        -5.1 -> .07136727723381077,
        -0.7 -> -4.273669982410843,
        1.0 -> 1.0,
        3.4347 -> 3.094563575747149,
        20.0 -> 1.216451004088318e17)

      foreach(1 to 20) { n => gamma(n) === (1L to (n - 1)).product }

      foreach(table) {
        case (x, res) =>
          gamma(x) aka s"gamma($x)" must beRelativelyCloseTo(res)
          logGamma(x) aka s"logGamma($x)" must beRelativelyCloseTo(log(gamma(x))) unless (res <= 0)
      }
    }

    "contain correct beta and logBeta implementations" in {
      val table = Map(
        (3.4, 2.6) -> .03551671602333521,
        (1.0, 1.0) -> 1.0,
        (0.1, 1.0) -> 10.0,
        (0.1, 10.0) -> 7.591380000911006,
        (17.6, 21.2) -> 2.009482117408006e-12)

      foreach(0.1 to 10.0 by 0.1) { p =>
        foreach(p to 10.0 by 0.1) { q =>
          beta(p, q) === beta(q, p)
          beta(p, q) must beRelativelyCloseTo(gamma(p) * gamma(q) / gamma(p + q))
        }
      }

      foreach(table) {
        case ((p, q), res) =>
          beta(p, q) aka s"beta($p,$q)" must beRelativelyCloseTo(res)
          logBeta(p, q) aka s"logBeta($p,$q)" must beRelativelyCloseTo(log(beta(p, q)))
      }
    }

    "contain correct factorial and logFactorial implementations" in {
      val logTable = Map(
        34 -> 88.58082754219768,
        79 -> 269.2910976510198,
        100 -> 363.7393755555635,
        134 -> 525.679013515995)

      foreach(1 to 20) { n =>
        fact(n) === (1L to n).product
        logFact(n) must beRelativelyCloseTo(log(fact(n)))
      }

      foreach(logTable) { case (n, res) => logFact(n) aka s"logFact($n)" must beRelativelyCloseTo(res) }
    }

    "contain correct binomial and logBinomial implementations" in {
      val logTable = Map(
        (1000, 456) -> 685.59413303061,
        (76, 33) -> 49.63358701698937,
        (4567, 1) -> 8.426611813185,
        (4567, 117) -> 540.939818970963)

      foreach(1 to 20) { n =>
        foreach(0 to n) { k =>
          binomial(n, k) === fact(n) / (fact(k) * fact(n - k))
          logBinomial(n, k) must beRelativelyCloseTo(log(binomial(n, k)))
        }
      }

      foreach(logTable) {
        case ((n, k), res) => logBinomial(n, k) aka s"logBinomial($n,$k)" must beRelativelyCloseTo(res)
      }
    }

    "contain correct realBinomial and logRealBinomial implementations" in {
      val table = Map(
        (4.7, 2) -> 8.695,
        (4.7, 4) -> 3.325837500000001,
        (3.3, 6) -> .004892387499999996,
        (0.1, 6) -> -0.0131620125,
        (2.0, 4) -> 0.0,
        (0.0, 2) -> 0.0,
        (0.0, 0) -> 1.0,
        (-0.1, 4) -> 0.0298375,
        (-1.0, 4) -> 1.0,
        (-1.0, 3) -> -1.0,
        (-1.2, 0) -> 1.0,
        (-6.3, 3) -> -63.61949999999999,
        (-6.3, 4) -> 147.9153375,
        (-7.0, 8) -> 3003.0)

      val logTable = Map(
        (1000.0, 456) -> 685.59413303061,
        (76.0, 33) -> 49.63358701698937,
        (4567.0, 1) -> 8.426611813185,
        (4567.0, 117) -> 540.939818970963,
        (545.45, 66) -> 198.2739579258497,
        (1333.5, 245) -> 632.5086289136327,
        (-1333.5, 24) -> 118.1145357524144)

      foreach(1 to 20) { n =>
        foreach(0 to n) { k =>
          realBinomial(n, k) must beRelativelyCloseTo(binomial(n, k))
          logRealBinomial(n, k) must beRelativelyCloseTo(logBinomial(n, k))
        }
      }

      foreach(table) {
        case ((x, k), res) =>
          realBinomial(x, k) aka s"realBinomial($x,$k)" must beRelativelyCloseTo(res)
          logRealBinomial(x, k) aka s"logRealBinomial($x,$k)" must beRelativelyCloseTo(log(realBinomial(x, k))) unless (res <= 0)
      }

      foreach(logTable) {
        case ((x, k), res) => logRealBinomial(x, k) aka s"logRealBinomial($x,$k)" must beRelativelyCloseTo(res)
      }
    }
  }
}
