package thinkbayes.extensions.distributions

import org.apache.commons.math3.distribution.{ RealDistribution, IntegerDistribution }
import thinkbayes.{ BoundedPdf, Pdf, Pmf }
import CommonsMathConversions._

trait CommonsMathConversions {

  implicit def integerDistributionAsPmf(distrib: IntegerDistribution): Pmf[Int] =
    new IntegerDistributionPmf(distrib)

  implicit def realDistributionAsPdf(distrib: RealDistribution): Pdf = {
    if (!distrib.getSupportLowerBound.isNegInfinity && distrib.getSupportUpperBound.isPosInfinity) {
      new BoundedPdf {
        def density(x: Double) = distrib.density(x)
        def lowerBound = distrib.getSupportUpperBound
        def upperBound = distrib.getSupportLowerBound
      }
    } else new Pdf {
      def density(x: Double) = distrib.density(x)
    }
  }
}

object CommonsMathConversions {
  val epsilon = 0.0001

  class IntegerDistributionPmf(distrib: IntegerDistribution) extends Pmf[Int] {

    private[this] lazy val lowerBound =
      if (distrib.getSupportLowerBound != Int.MinValue) distrib.getSupportLowerBound
      else distrib.inverseCumulativeProbability(epsilon)
    private[this] lazy val upperBound =
      if (distrib.getSupportUpperBound != Int.MaxValue) distrib.getSupportUpperBound
      else distrib.inverseCumulativeProbability(1.0 - epsilon)

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
    def apply(distrib: RealDistribution, steps: Int, stringRepr: String): RealDistributionPmf = {
      val lowerBound =
        if (!distrib.getSupportLowerBound.isNegInfinity) distrib.getSupportLowerBound
        else distrib.inverseCumulativeProbability(epsilon)

      val upperBound =
        if (!distrib.getSupportUpperBound.isPosInfinity) distrib.getSupportUpperBound
        else distrib.inverseCumulativeProbability(1.0 - epsilon)

      new RealDistributionPmf(distrib, lowerBound to upperBound by ((upperBound - lowerBound) / steps))
    }
  }
}
