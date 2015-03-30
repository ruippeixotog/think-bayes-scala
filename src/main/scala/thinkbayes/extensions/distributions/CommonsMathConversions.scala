package thinkbayes.extensions.distributions

import org.apache.commons.math3.distribution.{ IntegerDistribution, RealDistribution }
import thinkbayes.extensions.distributions.CommonsMathConversions._
import thinkbayes._

trait CommonsMathConversions {

  implicit def integerDistributionAsPmf(distrib: IntegerDistribution): Pmf[Int] =
    new IntegerDistributionPmf(distrib)

  implicit def realDistributionAsPdf(distrib: RealDistribution): Pdf = Pdf(distrib.density)
}

object CommonsMathConversions {
  val defaultCutoff = 0.0001

  def approximateIntegerLowerBound(distrib: IntegerDistribution, cutoff: Double = defaultCutoff) =
    if (distrib.getSupportLowerBound != Int.MinValue) distrib.getSupportLowerBound
    else distrib.inverseCumulativeProbability(defaultCutoff)

  def approximateIntegerUpperBound(distrib: IntegerDistribution, cutoff: Double = defaultCutoff) =
    if (distrib.getSupportUpperBound != Int.MaxValue) distrib.getSupportUpperBound
    else distrib.inverseCumulativeProbability(1.0 - defaultCutoff)

  def approximateRealLowerBound(distrib: RealDistribution, cutoff: Double = defaultCutoff) =
    if (!distrib.getSupportLowerBound.isNegInfinity) distrib.getSupportLowerBound
    else distrib.inverseCumulativeProbability(defaultCutoff)

  def approximateRealUpperBound(distrib: RealDistribution, cutoff: Double = defaultCutoff) =
    if (!distrib.getSupportUpperBound.isPosInfinity) distrib.getSupportUpperBound
    else distrib.inverseCumulativeProbability(1.0 - defaultCutoff)

  class IntegerDistributionPmf(distrib: IntegerDistribution) extends Pmf[Int] {
    private[this] lazy val lowerBound = approximateIntegerLowerBound(distrib)
    private[this] lazy val upperBound = approximateIntegerUpperBound(distrib)

    def +(kv: (Int, Double))(implicit dummy: DummyImplicit): Pmf[Int] = toHistogramPmf + kv
    def -(key: Int): Pmf[Int] = toHistogramPmf - key
    def get(key: Int) = Some(distrib.probability(key))
    def iterator = (lowerBound to upperBound).iterator.map { key => (key, distrib.probability(key)) }

    @inline private[this] def toHistogramPmf: Pmf[Int] = Pmf(iterator.toSeq: _*)
  }

  class RealDistributionPmf(distrib: RealDistribution, domain: Seq[Double]) extends Pmf[Double] {
    def +(kv: (Double, Double))(implicit dummy: DummyImplicit): Pmf[Double] = toHistogramPmf + kv
    def -(key: Double): Pmf[Double] = toHistogramPmf - key
    def get(key: Double) = Some(distrib.probability(key))
    def iterator = domain.iterator.map { key => (key, distrib.density(key)) }

    @inline private[this] def toHistogramPmf: Pmf[Double] = Pmf(iterator.toSeq: _*)
  }

  object RealDistributionPmf {
    def apply(distrib: RealDistribution, steps: Int): RealDistributionPmf = {
      val lowerBound = approximateRealLowerBound(distrib)
      val upperBound = approximateRealUpperBound(distrib)
      new RealDistributionPmf(distrib, lowerBound to upperBound by ((upperBound - lowerBound) / steps))
    }
  }
}
