package thinkbayes

import scala.util.Random

case class Pmf[K](hist: Map[K, Double]) {

  /**
   * Gets the probability associated with a given key.
   * @param key the key whose probability is to be returned
   * @return the probability associated with the given key.
   */
  def prob(key: K) = hist.getOrElse(key, 0.0)

  /**
   * Returns the value with the highest probability.
   * @return the value with the highest probability.
   */
  def max: K = hist.maxBy(_._2)._1

  /**
   * Computes the mean of the PMF.
   * @return the mean of the PMF.
   */
  def mean(implicit num: Numeric[K]): Double =
    hist.map { case (h, prob) => num.toDouble(h) * prob }.sum

  def random(): K = {
    def get(rand: Double, it: Iterator[(K, Double)]): K = {
      val (k, prob) = it.next()
      if(rand < prob) k else get(rand - prob, it)
    }
    get(Random.nextDouble() * hist.values.sum, hist.iterator)
  }

  /**
   * Normalizes this PMF so the sum of all probabilities is 1.0.
   */
  def normalized = {
    val sum = hist.values.sum
    Pmf(hist.mapValues(_ / sum))
  }

  def join[J](other: Pmf[K], comb: (K, K) => J)(implicit num: Numeric[K]): Pmf[J] = {
    val joinedHist = for {
      (k, prob) <- hist.toSeq
      (k2, prob2) <- other.hist
    } yield (comb(k, k2), prob * prob2)

    Pmf(joinedHist: _*)
  }

  def +(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other, num.plus)
  def -(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other, num.minus)

  def mixture[K2](implicit ev: K <:< Pmf[K2]): Pmf[K2] = {
    val mixHist = hist.foldLeft(Map.empty[K2, Double]) { case (acc, (outcome, weight)) =>
      outcome.hist.foldLeft(acc) { case (acc2, (k, prob)) =>
        acc2.updated(k, acc2.getOrElse(k, 0.0) + weight * prob)
      }
    }
    Pmf(mixHist).normalized
  }

  def toCdf(implicit ord: Ordering[K]): Cdf[K] = Cdf(hist.toSeq: _*)
}

object Pmf {

  def apply[K](probs: (K, Double)*): Pmf[K] = {
    val hist = probs.foldLeft(Map.empty[K, Double]) { case (acc, (k, prob)) =>
      acc.updated(k, acc.getOrElse(k, 0.0) + prob)
    }
    Pmf(hist).normalized
  }

  def apply[K](keys: TraversableOnce[K]): Pmf[K] = {
    val hist = keys.foldLeft(Map.empty[K, Double]) { case (acc, k) =>
      acc.updated(k, acc.getOrElse(k, 0.0) + 1.0)
    }
    Pmf(hist).normalized
  }
}
