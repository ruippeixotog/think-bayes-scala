package thinkbayes

import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecificationLike

trait PmfMatchers { this: SpecificationLike =>
  def beCloseTo[K](otherPmf: Pmf[K]): Matcher[Pmf[K]] = { pmf: Pmf[K] =>
    foreach(pmf.iterator) { case (k, prob) => pmf.prob(k) must beCloseTo(prob, 0.00001) }
  }
}
