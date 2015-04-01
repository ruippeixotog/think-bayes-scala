package thinkbayes.extensions

import scala.annotation.tailrec
import scala.util.Random
import thinkbayes._

object Stats {

  implicit class PmfStats[K](val pmf: Pmf[K]) extends AnyVal {

    def percentile(p: Double)(implicit ord: Ordering[K]): K = {
      def loop(remHist: Seq[(K, Double)], total: Double): K = remHist match {
        case (key, prob) +: Nil => key
        case (key, prob) +: rem if total + prob > p => key
        case (key, prob) +: rem => loop(rem, total + prob)
      }
      loop(pmf.toSeq.sorted, 0.0)
    }

    def credibleInterval(p: Double)(implicit ord: Ordering[K]): (K, K) =
      pmf.toCdf.credibleInterval(p)
  }

  implicit class PmfSampling[K](val pmf: Pmf[K]) extends AnyVal {

    // This implements the alias method as described in
    // http://www.keithschwarz.com/darts-dice-coins/
    //
    // This has a total initialization time of O(n) and generation time of O(1).
    def samplesIterator: Iterator[K] = {
      val len = pmf.hist.size
      val scale = len / pmf.hist.map(_._2).sum
      val scaled = pmf.hist.toList.map({ case (k, v) => k -> (v * scale) })
      val (small, large) = scaled.partition(_._2 < 1.0)

      @tailrec
      def alias(
        small: List[(K, Double)],
        large: List[(K, Double)],
        rest: List[(K, Double, Option[K])]): List[(K, Double, Option[K])] = {
        (small, large) match {
          case ((s, ps) :: ss, (l, pl) :: ll) =>
            val remainder = (l, pl - (1.0 - ps))
            val newRest = (s, ps, Some(l)) :: rest
            if (remainder._2 < 1)
              alias(remainder :: ss, ll, newRest)
            else
              alias(ss, remainder :: ll, newRest)

          case (_, (l, _) :: ll) =>
            alias(small, ll, (l, 1.0, None) :: rest)

          case ((s, _) :: ss, _) =>
            alias(ss, large, (s, 1.0, None) :: rest)

          case _ =>
            rest
        }
      }

      val table = Vector() ++ alias(small, large, Nil)
      def select(p1: Double, p2: Double, table: Vector[(K, Double, Option[K])]): K = {
        table((p1 * len).toInt) match {
          case (a, _, None) => a
          case (a, p, Some(b)) => if (p2 <= p) a else b
        }
      }

      Iterator.continually(select(Random.nextDouble(), Random.nextDouble(), table))
    }
  }

  implicit class CdfStats[K](val cdf: Cdf[K]) extends AnyVal {

    def percentile(prob: Double) = cdf.value(prob)

    def credibleInterval(p: Double): (K, K) = {
      val distTail = (1.0 - p) / 2.0
      (percentile(distTail), percentile(1.0 - distTail))
    }
  }
}
