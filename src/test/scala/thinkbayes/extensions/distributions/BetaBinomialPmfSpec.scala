package thinkbayes.extensions.distributions

import org.specs2.mutable.Specification
import thinkbayes.PmfMatchers

import scala.util.Random

class BetaBinomialPmfSpec extends Specification with PmfMatchers {

  "A BetaBinomialPmf" should {

    def randomBetaBinomialPmf(n: Int) = new BetaBinomialPmf(n, Random.nextDouble() * 20, Random.nextDouble() * 20)

    "define an iterator having all possible outcomes" in {
      foreach(0 to 100) { n =>
        randomBetaBinomialPmf(n).iterator.length === n + 1
      }
    }

    "define the mean as a constant-time closed form expression" in {
      val table = Map(
        (1234, 4.5, 3.5) -> 694.125,
        (31234, 1.0, 1.0) -> 15617.0,
        (31234, 0.4, 1.0) -> 8924.0,
        (31234, 1.0, 0.5) -> 20822.666666667,
        (31234, 1.4, 1.0) -> 18219.833333333,
        (31234, 1.0, 1.5) -> 12493.6,
        (31234, 0.4, 0.5) -> 13881.777777778,
        (31234, 0.4, 1.5) -> 6575.57894733406,
        (31234, 1.4, 0.5) -> 23014.526315792507,
        (31234, 1.4, 1.5) -> 15078.482758214723,
        (31234, 47.5, 92.5) -> 10597.3,
        (312349, 6.2, 52.5) -> 32990.9,
        (312364973, 10483.2, 24681.3) -> 9.312188385882352e7)

      foreach(0 to 100) { n =>
        val pmf = randomBetaBinomialPmf(n)
        pmf.mean must beRelativelyCloseTo(pmf.toCategoricalPmf.mean)
      }

      foreach(table) {
        case ((n, a, b), res) =>
          new BetaBinomialPmf(n, a, b).mean must beRelativelyCloseTo(res)
      }
    }

    "define the variance as a constant-time closed form expression" in {
      val table = Map(
        (1234, 4.5, 3.5) -> 41907.8,
        (31234, 1.0, 1.0) -> 8.130210200333697e7,
        (31234, 0.4, 1.0) -> 8.29597350004374e7,
        (31234, 1.0, 0.5) -> 8.67208539532947e7,
        (31234, 1.4, 1.0) -> 6.974534336258161e7,
        (31234, 1.0, 1.5) -> 6.690108623889974e7,
        (31234, 0.4, 0.5) -> 1.2678243879877478e8,
        (31234, 0.4, 1.5) -> 5.591491645461144e7,
        (31234, 1.4, 0.5) -> 6.5234069195641756e7,
        (31234, 1.4, 1.5) -> 6.2467514339533895e7,
        (31234, 47.5, 92.5) -> 1.5579648550531915e6,
        (312349, 6.2, 52.5) -> 1.5440533738189435e8,
        (312364973, 10483.2, 24681.3) -> 5.806437517739379e11)

      foreach(0 to 100) { n =>
        val pmf = randomBetaBinomialPmf(n)
        pmf.variance must beRelativelyCloseTo(pmf.toCategoricalPmf.variance)
      }

      foreach(table) {
        case ((n, a, b), res) =>
          new BetaBinomialPmf(n, a, b).variance must beRelativelyCloseTo(res)
      }
    }

    "define the mode as a constant-time closed form expression" in {
      val table = Map(
        (1234, 4.5, 3.5) -> 720,
        (31234, 1.0, 1.0) -> 0,
        (31234, 0.4, 1.0) -> 0,
        (31234, 1.0, 0.5) -> 31234,
        (31234, 1.4, 1.0) -> 31234,
        (31234, 1.0, 1.5) -> 0,
        (31234, 0.4, 0.5) -> 0,
        (31234, 0.4, 1.5) -> 0,
        (31234, 1.4, 0.5) -> 31234,
        (31234, 1.4, 1.5) -> 13882,
        (31234, 47.5, 92.5) -> 10524,
        (312349, 6.2, 52.5) -> 28645,
        (312364973, 10483.2, 24681.3) -> 93118643)

      foreach(0 to 100) { n =>
        val pmf = randomBetaBinomialPmf(n)
        pmf.variance must beRelativelyCloseTo(pmf.toCategoricalPmf.variance)
      }

      foreach(table) {
        case ((n, a, b), res) =>
          new BetaBinomialPmf(n, a, b).mode === res
      }
    }
  }
}
