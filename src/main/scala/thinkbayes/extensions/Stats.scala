package thinkbayes.extensions

import thinkbayes._

object Stats {

  implicit class PmfStats[K](val pmf: Pmf[K]) extends AnyVal {

    /**
      * Calculates a quantile of this `Pmf` according to the Nearest Rank definition.
      * @param p the quantile to calculate
      * @return the quantile of this `Pmf` according to the Nearest Rank definition.
      */
    def quantile(p: Double)(implicit ord: Ordering[K]): K = {
      val domain = pmf.keysIterator.toIndexedSeq.sorted

      def loop(curr: Int, currProb: Double): K =
        if (curr >= domain.length - 1) domain(curr)
        else {
          val prob = pmf.prob(domain(curr))
          if (currProb + prob >= p) domain(curr)
          else loop(curr + 1, currProb + prob)
        }

      loop(0, 0.0)
    }

    def credibleInterval(p: Double)(implicit ord: Ordering[K]): (K, K) =
      pmf.toCdf.credibleInterval(p)
  }

  implicit class CdfStats[K](val cdf: Cdf[K]) extends AnyVal {

    /**
      * Calculates a quantile of this `Cdf` according to the Nearest Rank definition.
      * @param p the quantile to calculate
      * @return the quantile of this `Cdf` according to the Nearest Rank definition.
      */
    def quantile(p: Double) = cdf.value(p)

    def credibleInterval(p: Double): (K, K) = {
      val distTail = (1.0 - p) / 2.0
      (quantile(distTail), quantile(1.0 - distTail))
    }
  }
}
