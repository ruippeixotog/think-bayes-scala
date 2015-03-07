package thinkbayes.extensions.distributions

import org.apache.commons.math3.distribution.{ RealDistribution, IntegerDistribution }
import thinkbayes.{ BoundedPdf, Pdf, Pmf }
import CommonsMathConversions._

trait CommonsMathConversions {

  implicit def integerDistributionAsPmf(distrib: IntegerDistribution): Pmf[Int] =
    Pmf(IntegerDistributionMapView(distrib))

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

  case class IntegerDistributionMapView(distrib: IntegerDistribution) extends Map[Int, Double] {

    private[this] lazy val lowerBound =
      if (distrib.getSupportLowerBound != Int.MinValue) distrib.getSupportLowerBound
      else distrib.inverseCumulativeProbability(epsilon)
    private[this] lazy val upperBound =
      if (distrib.getSupportUpperBound != Int.MaxValue) distrib.getSupportUpperBound
      else distrib.inverseCumulativeProbability(1.0 - epsilon)

    def +[B1 >: Double](kv: (Int, B1)) = iterator.toMap + kv
    def -(key: Int) = iterator.toMap - key

    def get(key: Int) = Some(distrib.probability(key))
    def iterator = (lowerBound to upperBound).iterator.map { key => (key, distrib.probability(key)) }
  }

  case class RealDistributionMapView(distrib: RealDistribution, domain: Seq[Double]) extends Map[Double, Double] {
    def +[B1 >: Double](kv: (Double, B1)) = iterator.toMap + kv
    def -(key: Double) = iterator.toMap - key

    def get(key: Double) = Some(distrib.probability(key))
    def iterator = domain.iterator.map { key => (key, distrib.density(key)) }
  }

  object RealDistributionMapView {
    def apply(distrib: RealDistribution, steps: Int): RealDistributionMapView = {
      val lowerBound =
        if (!distrib.getSupportLowerBound.isNegInfinity) distrib.getSupportLowerBound
        else distrib.inverseCumulativeProbability(epsilon)

      val upperBound =
        if (!distrib.getSupportUpperBound.isPosInfinity) distrib.getSupportUpperBound
        else distrib.inverseCumulativeProbability(1.0 - epsilon)

      this(distrib, lowerBound to upperBound by ((upperBound - lowerBound) / steps))
    }
  }
}
