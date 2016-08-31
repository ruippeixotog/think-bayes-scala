package thinkbayes.extensions.distributions

import thinkbayes._

/**
  * Trait of `Pmf` instances whose probability value is calculated by a closed-form expression and, as such, do not allow
  * freely adding, removing or modifying outcomes and probabilities. Such operations create and return a new categorical
  * distribution reflecting the changes.
  *
  * Typically, implementations of `ClosedFormPmf` also have a closed-form expression for calculating their numerical
  * mean. If that is the case, they are expected to override `mean` to provide an efficient implementation.
  *
  * @tparam K the type of the outcomes
  */
trait ClosedFormPmf[K] extends PmfLike[K, Pmf[K]] {
  def +(kv: (K, Double))(implicit dummy: DummyImplicit) = toCategoricalPmf + kv
  def -(key: K) = toCategoricalPmf - key
}
