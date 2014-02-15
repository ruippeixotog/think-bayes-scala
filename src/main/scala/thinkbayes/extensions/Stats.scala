package thinkbayes.extensions

import thinkbayes._

object Stats {

  implicit class PmfStats[K](val pmf: Pmf[K]) extends AnyVal {

    def percentile(p: Double)(implicit ord: Ordering[K]): K = {
      def loop(remHist: Seq[(K, Double)], total: Double): K = remHist match {
        case (key, prob) +: Nil => key
        case (key, prob) +: rem if total + prob > p => key
        case (key, prob) +: rem => loop(rem, total + prob)
      }
      loop(pmf.hist.toSeq.sorted, 0.0)
    }

    def credibleInterval(p: Double)(implicit ord: Ordering[K]): (K, K) =
      pmf.toCdf.credibleInterval(p)
  }

  implicit class CdfStats[K](val cdf: Cdf[K]) extends AnyVal {

    def percentile(prob: Double) = cdf.value(prob)

    def credibleInterval(p: Double): (K, K) = {
      val distTail = (1.0 - p) / 2.0
      (percentile(distTail), percentile(1.0 - distTail))
    }
  }
}
