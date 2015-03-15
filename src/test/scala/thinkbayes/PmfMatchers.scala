package thinkbayes

import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecificationLike

trait PmfMatchers { this: SpecificationLike =>
  def beCloseTo[K](otherPmf: Pmf[K], delta: Double = 0.00001): Matcher[Pmf[K]] = { pmf: Pmf[K] =>
    foreach(pmf.iterator) {
      case (k, prob) => prob aka s"The probability for $k" must beCloseTo(otherPmf.prob(k), delta)
    }
    foreach(otherPmf.iterator) {
      case (k, prob) => pmf.prob(k) aka s"The probability for $k" must beCloseTo(prob, delta)
    }
  }
}
