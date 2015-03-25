package thinkbayes.extensions

import org.apache.commons.math3.distribution._
import org.apache.commons.math3.random.Well19937c
import thinkbayes._
import thinkbayes.extensions.distributions.CommonsMathConversions
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

  def exponentialPmf(lam: Double, high: Double = Double.PositiveInfinity, steps: Int = 2000): Pmf[Double] = {
    val distrib = new ExponentialDistribution(rndGen, 1.0 / lam)
    val realHigh = if (high.isPosInfinity) distrib.inverseCumulativeProbability(1.0 - epsilon) else high

    new RealDistributionPmf(distrib, 0.0 to realHigh by (realHigh / steps))
  }

  def binomialPmf(trials: Int, p: Double): Pmf[Int] = new BinomialDistribution(rndGen, trials, p)

  def hypergeometricPmf(popSize: Int, successCount: Int, sampleSize: Int): Pmf[Int] =
    if (sampleSize == 0 || successCount == 0) Pmf(0 -> 1.0)
    else if (popSize == successCount) Pmf(sampleSize -> 1.0)
    else new HypergeometricDistribution(rndGen, popSize, successCount, sampleSize)
}
