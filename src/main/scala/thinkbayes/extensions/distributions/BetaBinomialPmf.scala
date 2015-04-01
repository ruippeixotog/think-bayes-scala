package thinkbayes.extensions.distributions

import org.apache.commons.math3.special.Gamma._
import org.apache.commons.math3.util.FastMath
import thinkbayes.Pmf

/**
 * A `Pmf` of a beta-binomial distribution ([[http://en.wikipedia.org/wiki/Beta-binomial_distribution]]).
 * @param trials the number of trials
 * @param alpha the alpha parameter
 * @param beta the beta parameter
 */
class BetaBinomialPmf(trials: Int, alpha: Double, beta: Double) extends Pmf[Int] with ClosedFormPmf[Int] {

  private[this] def logP(k: Int) =
    (logGamma(k + alpha) + logGamma(trials - k + beta) + logGamma(alpha + beta) + logGamma(trials + 2)) -
      (FastMath.log(trials + 1) + logGamma(alpha + beta + trials) +
        logGamma(alpha) + logGamma(beta) + logGamma(k + 1) + logGamma(trials - k + 1))

  @inline private[this] def p(k: Int) = FastMath.exp(logP(k))

  def get(key: Int) = if (key < 0 || key > trials) None else Some(p(key))
  def iterator = Iterator.tabulate(trials + 1) { key => (key, p(key)) }

  override def mean(implicit num: Numeric[Int]) = trials * alpha / (alpha + beta)
}
