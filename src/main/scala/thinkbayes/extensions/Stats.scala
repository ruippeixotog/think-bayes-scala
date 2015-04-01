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
      def loop(remHist: Seq[(K, Double)], total: Double): K = remHist match {
        case (key, _) +: Nil => key
        case (key, prob) +: rem if total + prob >= p => key
        case (_, prob) +: rem => loop(rem, total + prob)
      }
      loop(pmf.toSeq.sorted, 0.0)
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
