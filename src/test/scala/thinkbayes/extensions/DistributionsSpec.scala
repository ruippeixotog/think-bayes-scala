package thinkbayes.extensions

import org.specs2.mutable.Specification
import thinkbayes._
import thinkbayes.extensions.Distributions._

class DistributionsSpec extends Specification with PmfMatchers {

  "The Distributions extension" should {

    "provide a way to create Pdfs for normal distributions" in {
      todo
    }

    "provide a way to create approximate Pmfs for normal distributions" in {
      todo
    }

    "provide a way to create Pmfs for Poisson distributions" in {
      def poissonFunc(lam: Double, k: Int) = math.pow(lam, k) * math.exp(-lam) / (1 to k).product

      poissonPmf(1.5).filterKeys(_ <= 5) must beCloseTo((0 to 5).map { k => (k, poissonFunc(1.5, k)) }.toMap.toPmf)
      foreach(Seq(0.1, 1.0, 3.9)) { lam => poissonPmf(lam).mean must beCloseTo(lam, 0.001) }
    }

    "provide a way to create Pdfs for exponential distributions" in {
      todo
    }

    "provide a way to create approximate Pmfs for exponential distributions" in {
      todo
    }

    "provide a way to create Pmfs for binomial distributions" in {
      binomialPmf(2, 0.6) must beCloseTo(Pmf(0 -> 0.16, 1 -> 0.48, 2 -> 0.36))
      binomialPmf(1, 0.6) must beCloseTo(Pmf(0 -> 0.4, 1 -> 0.6))
      binomialPmf(0, 0.6) must beCloseTo(Pmf(0 -> 1.0))

      binomialPmf(100, 1.0) must beCloseTo(Pmf(100 -> 1.0))
      binomialPmf(100, 0.0) must beCloseTo(Pmf(0 -> 1.0))
    }

    "provide a way to create Pmfs for hypergeometric distributions" in {
      hypergeometricPmf(10, 2, 9) must beCloseTo(Pmf(1 -> 0.2, 2 -> 0.8))
      hypergeometricPmf(10, 2, 2) must beCloseTo(Pmf(0 -> 28.0 / 45, 1 -> 16.0 / 45, 2 -> 1.0 / 45))
      hypergeometricPmf(10, 2, 1) must beCloseTo(Pmf(0 -> 0.8, 1 -> 0.2))
      hypergeometricPmf(10, 2, 0) must beCloseTo(Pmf(0 -> 1.0))

      hypergeometricPmf(100, 100, 100) must beCloseTo(Pmf(100 -> 1.0))
      hypergeometricPmf(100, 100, 50) must beCloseTo(Pmf(50 -> 1.0))
      hypergeometricPmf(100, 100, 49) must beCloseTo(Pmf(49 -> 1.0))
      hypergeometricPmf(100, 100, 0) must beCloseTo(Pmf(0 -> 1.0))
      hypergeometricPmf(100, 50, 100) must beCloseTo(Pmf(50 -> 1.0))
      hypergeometricPmf(100, 0, 50) must beCloseTo(Pmf(0 -> 1.0))
      hypergeometricPmf(100, 0, 1) must beCloseTo(Pmf(0 -> 1.0))
      hypergeometricPmf(100, 0, 0) must beCloseTo(Pmf(0 -> 1.0))
      hypergeometricPmf(0, 0, 0) must beCloseTo(Pmf(0 -> 1.0))
    }

    "provide a way to create bounded Pdfs for beta distributions" in {
      val epsilon = 0.00001

      betaPdf(3.5, 4.7).lowerBound === 0.0
      betaPdf(6.8, 10.1).upperBound === 1.0
      forall(0.0 to 1.0 by 0.1) { p => betaPdf(1.0, 1.0).density(p) === 1.0 }
      forall(0.0 to 1.0 by 0.1) { p => betaPdf(2.0, 1.0).density(p) must beCloseTo(2 * p, epsilon) }
      forall(0.0 to 1.0 by 0.1) { p => betaPdf(1.0, 2.0).density(p) must beCloseTo(2 * (1 - p), epsilon) }
      betaPdf(9.4, 2.3).density(0.45) must beCloseTo(172.285 * math.pow(0.45, 8.4) * math.pow(0.55, 1.3), epsilon)
      betaPdf(9.4, 2.3).density(0.78) must beCloseTo(172.285 * math.pow(0.78, 8.4) * math.pow(0.22, 1.3), epsilon)
    }

    "provide a way to estimate a Pdf from a sequence of samples" in {
      todo
    }
  }
}
