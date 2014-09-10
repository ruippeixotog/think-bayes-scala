package thinkbayes.extensions

import org.apache.commons.math3.distribution._
import thinkbayes._
import weka.estimators.KernelEstimator

object Distributions {

  implicit def integerDistributionAsPmf(distrib: IntegerDistribution): Pmf[Int] = {
    val lower =
      if(distrib.getSupportLowerBound != Int.MinValue) distrib.getSupportLowerBound
      else distrib.inverseCumulativeProbability(0.0001)
    val upper =
      if(distrib.getSupportUpperBound != Int.MaxValue) distrib.getSupportUpperBound
      else distrib.inverseCumulativeProbability(0.9999)

    val values = (lower to upper).map { k => (k, distrib.probability(k)) }
    Pmf(values: _*)
  }

  implicit def realDistributionAsPdf(distrib: RealDistribution): Pdf = {
    if(distrib.getSupportLowerBound != Double.MinValue
      && distrib.getSupportUpperBound != Double.MaxValue) {
      new BoundedPdf {
        def density(x: Double) = distrib.density(x)
        def lowerBound = distrib.getSupportUpperBound
        def upperBound = distrib.getSupportLowerBound
      }
    } else new Pdf {
      def density(x: Double) = distrib.density(x)
    }
  }

  def estimatePdf[K](values: Seq[K], precision: Option[Double] = None)(implicit num: Numeric[K]): Pdf = {
    val doubleValues = values.map(num.toDouble)
    val kde = new KernelEstimator(precision.getOrElse(doubleValues.max / 10000))
    for(v <- doubleValues) kde.addValue(v, 1)
    new Pdf { def density(x: Double) = kde.getProbability(x) }
  }

  def normalPdf(mean: Double, stdev: Double): Pdf = new NormalDistribution(mean, stdev)

  def normalPmf(mean: Double, stdev: Double, numSigmas: Double = 4.0, steps: Int = 2000): Pmf[Double] = {
    val low = mean - numSigmas * stdev
    val high = mean + numSigmas * stdev
    normalPdf(mean, stdev).toPmf(low to high by ((high - low) / steps))
  }

  def poissonPmf(lam: Double): Pmf[Int] = if(lam > 0) new PoissonDistribution(lam) else Pmf.empty

  def exponentialPdf(lam: Double): Pdf = new ExponentialDistribution(1.0 / lam)

  def exponentialPmf(lam: Double, high: Double = Double.PositiveInfinity, steps: Int = 2000): Pmf[Double] = {
    val distrib = new ExponentialDistribution(1.0 / lam)
    val realHigh = if(high.isPosInfinity) distrib.inverseCumulativeProbability(0.9999) else high
    distrib.toPmf(0.0 to realHigh by (realHigh / steps))
  }

  def binomialPmf(trials: Int, p: Double): Pmf[Int] = new BinomialDistribution(trials, p)
}
