package thinkbayes.extensions.distributions

import thinkbayes._

/**
 * Trait of `Pmf` instances whose probability value is calculated by a closed-form expression and, as such, do not allow
 * freely adding, removing or modifying outcomes and probabilities. Such operations create and return a new histogram
 * `Pmf` with the changes.
 *
 * @tparam K the type of the outcomes
 */
trait ClosedFormPmf[K] extends PmfLike[K, Pmf[K]] {
  def +(kv: (K, Double))(implicit dummy: DummyImplicit) = toHistogramPmf + kv
  def -(key: K) = toHistogramPmf - key
}
