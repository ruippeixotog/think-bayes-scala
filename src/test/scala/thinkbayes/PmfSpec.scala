package thinkbayes

import org.specs2.mutable.Specification

class PmfSpec extends Specification with PmfMatchers {

  "A Pmf" should {

    "have a factory method for constructing an empty one" in {
      Pmf.empty[Char].toMap ==== Map.empty[Char, Double]
    }

    "have a factory method receiving outcomes and respective probabilities" in {
      Pmf("a" -> 0.2, "b" -> 0.2, "c" -> 0.6).toMap === Map("a" -> 0.2, "b" -> 0.2, "c" -> 0.6)
      Pmf('H' -> 0.4, 'T' -> 0.6).toMap === Map('H' -> 0.4, 'T' -> 0.6)

      Pmf(Map("a" -> 0.2, "b" -> 0.2, "c" -> 0.6)).toMap === Map("a" -> 0.2, "b" -> 0.2, "c" -> 0.6)
      Pmf(Map('H' -> 0.4, 'T' -> 0.6)).toMap === Map('H' -> 0.4, 'T' -> 0.6)
    }

    "have a factory method receiving a sequence of possible outcomes" in {
      Pmf(1 to 4).toMap === Map(1 -> 0.25, 2 -> 0.25, 3 -> 0.25, 4 -> 0.25)
      Pmf(Seq('H', 'T')).toMap === Map('H' -> 0.5, 'T' -> 0.5)
    }

    "have a factory method receiving a sequence of samples" in {
      Pmf(Seq(1, 1, 2, 2, 3, 3, 3)).toMap === Map(1 -> 2.0 / 7, 2 -> 2.0 / 7, 3 -> 3.0 / 7)
      Pmf(Seq(true, true, false)).toMap === Map(true -> 2 / 3.0, false -> 1 / 3.0)
    }

    "allow retrieving the probability of an outcome or a set of outcomes" in {
      val pmf = Pmf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
      pmf.prob('a') === 0.2
      pmf.prob(_ < 'c') === 0.4
    }

    "allow retrieving the outcome with maximum probability (mode)" in {
      val pmf = Pmf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
      pmf.maxProb === ('c', 0.6)
      pmf.mode === 'c'
    }

    "allow calculating its mean when its outcomes are numeric" in {
      Pmf(0 -> 0.2, 1 -> 0.2, 2 -> 0.6).mean === 0.2 + 2 * 0.6
      Pmf(2.0 -> 0.5, 3.0 -> 0.5).mean === 2.5
    }

    "allow taking random samples from it" in {
      val nRuns = 10000
      val pmf = Pmf(0 -> 0.4, 1 -> 0.6)
      val samplePmf = Pmf(Iterator.fill(nRuns)(pmf.sample()))

      // Warning: this test will fail approximately one every billion runs.
      // You can check the failure probability using this very library:
      //   scala> binomialPmf(10000, 0.6).credibleInterval(1.0 - 1.0 / 1e9)
      //   res7: (Int, Int) = (5700,6298)
      samplePmf.prob(1) must beBetween(0.57, 0.63)
    }

    "allow re-normalizing the result to keep the sum of all probabilities equal to 1.0" in {
      val pmf = Pmf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
      pmf.filterKeys(_ != 'a') must beCloseTo(Pmf(Map('b' -> 0.2, 'c' -> 0.6)))
      pmf.filterKeys(_ != 'a').normalized must beCloseTo(Pmf('b' -> 0.25, 'c' -> 0.75))
    }

    "provide Map-like methods that keep the Pmf original type when applicable" in {
      val pmf = Pmf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)

      (pmf - 'a').normalized must beCloseTo(Pmf('b' -> 0.25, 'c' -> 0.75))
      (pmf + ('d' -> 1.0)).normalized must beCloseTo(Pmf('a' -> 0.1, 'b' -> 0.1, 'c' -> 0.3, 'd' -> 0.5))

      pmf.map { case (k, v) => ((k + 1).toChar, v) } ==== Pmf('b' -> 0.2, 'c' -> 0.2, 'd' -> 0.6)
      pmf.map { case (k, v) => ((k + 1).toChar, k) } ==== Map('b' -> 'a', 'c' -> 'b', 'd' -> 'c')
      pmf.mapKeys { k => (k + 1).toChar } ==== Pmf('b' -> 0.2, 'c' -> 0.2, 'd' -> 0.6)

      pmf.filter(_._1 == 'a').normalized ==== Pmf('a' -> 1.0)
      pmf.filterKeys(_ != 'a').normalized must beCloseTo(Pmf('b' -> 0.25, 'c' -> 0.75))

      pmf.toSet ==== Set('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
    }

    "allow being summed or subtracted by another when their outcomes are numeric" in {
      val d6 = Pmf(1 to 6)
      d6 ++ d6 must beCloseTo(Pmf(for { i <- 1 to 6; j <- 1 to 6 } yield i + j))
      d6 -- d6 must beCloseTo(Pmf(for { i <- 1 to 6; j <- 1 to 6 } yield i - j))
    }

    "allow being combined with another using a custom join function" in {
      val d6 = Pmf(1 to 6)
      d6.join(d6, math.max) must beCloseTo(Pmf(for { i <- 1 to 6; j <- 1 to 6 } yield math.max(i, j)))

      val coin = Pmf('H' -> 0.4, 'T' -> 0.6)
      coin.join(coin, _.toString + _) must beCloseTo(Pmf("HH" -> 0.16, "HT" -> 0.24, "TH" -> 0.24, "TT" -> 0.36))
    }

    "allow being flattened (mixtured) if its keys are also Pmfs" in {
      def die(n: Int) = Pmf(1 to n)
      val bag = Pmf(Seq(die(4), die(6))) // a bag containing 2 different dice

      val expectedMix = Pmf((for { dieN <- List(4, 6); i <- 1 to dieN } yield i -> 0.5 / dieN): _*)
      bag.mixture must beCloseTo(expectedMix) // roll of a random die from the bag
    }

    "allow being converted into a Cdf" in {
      val pmf = Pmf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
      pmf.toCdf === Cdf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
      pmf.toCdf.iterator.toSeq === Seq('a' -> 0.2, 'b' -> 0.4, 'c' -> 1.0)
    }
  }
}
