package thinkbayes.extensions

import org.apache.commons.math3.distribution._
import org.apache.commons.math3.random.Well19937c
import thinkbayes._
import thinkbayes.extensions.distributions._
import thinkbayes.extensions.distributions.CommonsMathConversions._
import weka.estimators.KernelEstimator

object Distributions extends CommonsMathConversions {
  private[this] val rndGen = new Well19937c()

  def estimatePdf[K](values: Seq[K], precision: Option[Double] = None)(implicit num: Numeric[K]): Pdf = {
    val doubleValues = values.map(num.toDouble)
    val kde = new KernelEstimator(precision.getOrElse(doubleValues.max / 10000))
    for (v <- doubleValues) kde.addValue(v, 1)
    new Pdf { def density(x: Double) = kde.getProbability(x) }
  }

  def normalPdf(mean: Double, stdev: Double): Pdf = new NormalDistribution(rndGen, mean, stdev)

  def normalPmf(mean: Double, stdev: Double, numSigmas: Double = 4.0, steps: Int = 2000): Pmf[Double] = {
    val low = mean - numSigmas * stdev
    val high = mean + numSigmas * stdev
    new RealDistributionPmf(new NormalDistribution(rndGen, mean, stdev), low to high by ((high - low) / steps))
  }

  def poissonPmf(lam: Double): Pmf[Int] =
    if (lam <= 0) Pmf.empty
    else new PoissonDistribution(rndGen, lam,
      PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)

  def exponentialPdf(lam: Double): Pdf = new ExponentialDistribution(rndGen, 1.0 / lam)

  def exponentialPmf(
    lam: Double,
    steps: Int = 2000,
    cutoff: Double = defaultCutoff,
    absCutoff: Double = Double.PositiveInfinity): Pmf[Double] = {

    val distrib = new ExponentialDistribution(rndGen, 1.0 / lam)
    val high = if (absCutoff.isPosInfinity) approximateRealUpperBound(distrib, cutoff) else absCutoff
    new RealDistributionPmf(distrib, 0.0 to high by (high / steps))
  }

  def binomialPmf(trials: Int, p: Double): Pmf[Int] = new BinomialDistribution(rndGen, trials, p)

  def hypergeometricPmf(popSize: Int, successCount: Int, sampleSize: Int): Pmf[Int] =
    if (sampleSize == 0 || successCount == 0) Pmf(0 -> 1.0)
    else if (popSize == successCount) Pmf(sampleSize -> 1.0)
    else new HypergeometricDistribution(rndGen, popSize, successCount, sampleSize)

  def betaPdf(alpha: Double, beta: Double): BoundedPdf = {
    val distrib = new BetaDistribution(alpha, beta)

    if (alpha < 1.0 || beta < 1.0) Pdf(0.0, 1.0)(distrib.density)
    else Pdf(0.0, 1.0) {
      case 0.0 => if (alpha == 1.0) beta else 0.0
      case 1.0 => if (beta == 1.0) alpha else 0.0
      case x => distrib.density(x)
    }
  }

  def betaBinomialPmf(trials: Int, alpha: Double, beta: Double) = new BetaBinomialPmf(trials, alpha, beta)
}
