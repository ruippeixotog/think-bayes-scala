package thinkbayes.extensions

import scala.annotation.tailrec
import scala.util.Random
import thinkbayes.Pmf

object Sampling {

  def randomJoin[K, J](pmfs: TraversableOnce[Pmf[K]], join: TraversableOnce[K] => J): J =
    join(pmfs.map(_.sample()))

  def sampleJoin[K, J](pmfs: TraversableOnce[Pmf[K]], n: Int, join: TraversableOnce[K] => J) =
    Pmf(Seq.fill(n)(randomJoin(pmfs, join)))

  def randomSum[K: Numeric](pmfs: TraversableOnce[Pmf[K]]) = randomJoin[K, K](pmfs, _.sum)
  def sampleSum[K: Numeric](pmfs: TraversableOnce[Pmf[K]], n: Int) = sampleJoin[K, K](pmfs, n, _.sum)
  def randomMax[K: Ordering](pmfs: TraversableOnce[Pmf[K]]): K = randomJoin[K, K](pmfs, _.max)
  def sampleMax[K: Ordering](pmfs: TraversableOnce[Pmf[K]], n: Int) = sampleJoin[K, K](pmfs, n, _.max)

  /**
    * Adds sampling extensions to `Pmf`
    */
  implicit class PmfSampling[K](val pmf: Pmf[K]) extends AnyVal {

    /**
      * This implements the alias method as described in
      * http://www.keithschwarz.com/darts-dice-coins/
      *
      * It has a total initialization time of O(n) and generation time of O(1).
      *
      * @return an infinite iterator of samples randomly drawn from the pmf.
      */
    def samplesIterator: Iterator[K] = {
      val len = pmf.size
      val scale = len / pmf.values.sum
      val scaled = pmf.toList.map({ case (k, v) => k -> (v * scale) })
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
}
