package thinkbayes.extensions.distributions

import org.apache.commons.math3.util.FastMath._
import thinkbayes._
import SpecialFunctions._

/**
 * A `Pmf` of a beta-binomial distribution ([[http://en.wikipedia.org/wiki/Beta-binomial_distribution]]).
 * @param trials the number of trials
 * @param alpha the alpha parameter
 * @param beta the beta parameter
 */
class BetaBinomialCdf(trials: Int, alpha: Double, beta: Double) extends Cdf[Int] {

  def logPoch(x: Double, k: Int) = log(abs(realBinomial(x + k - 1, k))) + logFact(k)
  // def logPoch(x: Double, k: Int) = logRealBinomial(x + k - 1, k) + logFact(k)

  def log3F2k(a: Double, b: Double, c: Double, d: Double, e: Double, k: Int) =
    logPoch(a, k) + logPoch(b, k) + logPoch(c, k) - (logPoch(d, k) + logPoch(e, k) + logFact(k))

  def log3F2(a: Double, b: Double, c: Double, d: Double, e: Double) =
    log((0 to 30).iterator.map { k => exp(log3F2k(a, b, c, d, e, k)) }.sum)

  def logP(k: Int) = logBeta(beta + trials - k - 1, alpha + k + 1) +
    log3F2(1, alpha + k + 1, -trials + k + 1, k + 2, -beta - trials + k + 2) -
    (logBeta(alpha, beta) + logBeta(trials - k, k + 2) + log(trials + 1))

  @inline private[this] def p(k: Int) = 1 - exp(logP(k))

  def prob(key: Int): Double = p(key)
  def value(prob: Double): Int = ???
  def iterator = Iterator.tabulate(trials + 1) { key => (key, p(key)) }

  override def toPmf = new BetaBinomialPmf(trials, alpha, beta)
}
