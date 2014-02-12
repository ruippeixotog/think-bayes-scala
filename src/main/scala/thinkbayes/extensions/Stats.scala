package thinkbayes.extensions

import thinkbayes.Pmf

object Stats {

  implicit class PmfStats[K](val pmf: Pmf[K]) extends AnyVal {

    def percentile(p: Double)(implicit ord: Ordering[K]): K = {
      def loop(remHist: Seq[(K, Double)], total: Double): K = remHist match {
        case (h, prob) +: rem if total + prob > p => h
        case (h, prob) +: rem => loop(rem, total + prob)
        case Nil => pmf.normalize(); pmf.percentile(p)
      }
      loop(pmf.hist.toSeq.sorted, 0.0)
    }

    def credibleInterval(p: Double)(implicit ord: Ordering[K]): (K, K) = {
      val distTail = (1.0 - p) / 2.0
      (percentile(distTail), percentile(1.0 - distTail))
    }
  }
}
