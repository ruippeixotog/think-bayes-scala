package thinkbayes

import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecificationLike

trait PmfMatchers { this: SpecificationLike =>
  val defaultEpsilon = 0.00001

  def beRelativelyCloseTo(expected: Double) = beCloseTo(expected, math.abs(expected) * defaultEpsilon)

  def beCloseTo[K](otherPmf: Pmf[K]): Matcher[Pmf[K]] = { pmf: Pmf[K] =>
    foreach(pmf.iterator) {
      case (k, prob) => prob aka s"The probability for $k" must beRelativelyCloseTo(otherPmf.prob(k))
    }
    foreach(otherPmf.iterator) {
      case (k, prob) => pmf.prob(k) aka s"The probability for $k" must beRelativelyCloseTo(prob)
    }
  }
}
