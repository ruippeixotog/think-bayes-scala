package thinkbayes.extensions

import org.apache.commons.math3.distribution._
import thinkbayes._
import weka.estimators.KernelEstimator

object Distributions {

  implicit def integerDistributionAsPmf(distrib: IntegerDistribution): Pmf[Int] = {
    val values = (distrib.getSupportLowerBound to distrib.getSupportUpperBound).map {
      k => (k, distrib.probability(k))
    }
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
}
