package thinkbayes.extensions.distributions

import org.apache.commons.math3.special.Gamma._
import org.apache.commons.math3.util.FastMath._
import thinkbayes.Pmf

/**
 * A `Pmf` of a beta-binomial distribution ([[http://en.wikipedia.org/wiki/Beta-binomial_distribution]]).
 * @param trials the number of trials
 * @param alpha the alpha parameter
 * @param beta the beta parameter
 */
class BetaBinomialPmf(val trials: Int, val alpha: Double, val beta: Double) extends Pmf[Int] with ClosedFormPmf[Int] {

  private[this] def logP(k: Int) =
    (logGamma(k + alpha) + logGamma(trials - k + beta) + logGamma(alpha + beta) + logGamma(trials + 2)) -
      (log(trials + 1) + logGamma(alpha + beta + trials) +
        logGamma(alpha) + logGamma(beta) + logGamma(k + 1) + logGamma(trials - k + 1))

  @inline private[this] def p(k: Int) = exp(logP(k))

  def get(key: Int) = if (key < 0 || key > trials) None else Some(p(key))
  def iterator = Iterator.tabulate(trials + 1) { key => (key, p(key)) }

  override def maxProb = { val m = mode; (m, prob(m)) }

  override def mode =
    if (alpha > 1.0 && beta > 1.0) round(trials * (alpha - 1.0) / (alpha + beta - 2.0)).toInt
    else if (alpha == 1.0 && beta == 1.0) 0 // or any other value in the range [0, `trials`]
    else if (alpha < 1.0 && beta < 1.0) 0 // or `trials`
    else if (alpha < 1.0 && beta >= 1.0 || alpha == 1.0 && beta > 1.0) 0
    else trials

  override def mean(implicit num: Numeric[Int]) = trials * alpha / (alpha + beta)

  override def variance(implicit num: Numeric[Int]) =
    trials * alpha * beta * (alpha + beta + trials) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1))
}
