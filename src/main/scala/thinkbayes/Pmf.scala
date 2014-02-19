package thinkbayes

import scala.util.Random

class Pmf[K] {
  var hist = Map[K, Double]()

  /**
   * Gets the probability associated with a given key.
   * @param key the key whose probability is to be returned
   * @return the probability associated with the given key.
   */
  def prob(key: K) = hist.getOrElse(key, 0.0)

  def set(key: K, prob: Double) = hist += (key -> prob)
  def incr(key: K, prob: Double = 1.0) = hist += (key -> (hist.getOrElse(key, 0.0) + prob))
  def mult(key: K, factor: Double) = hist += (key -> (hist.getOrElse(key, 0.0) * factor))

  /**
   * Returns the value with the highest probability.
   * @return the value with the highest probability.
   */
  def max: K = hist.maxBy(_._2)._1

  def random(): K = {
    def get(rand: Double, it: Iterator[(K, Double)]): K = {
      val (k, prob) = it.next()
      if(rand < prob) k else get(rand - prob, it)
    }
    get(Random.nextDouble() * hist.values.sum, hist.iterator)
  }

  def join[J](other: Pmf[K], comb: (K, K) => J)(implicit num: Numeric[K]): Pmf[J] = {
    val pmf = new Pmf[J]
    for {
      (k, prob) <- hist
      (k2, prob2) <- other.hist
    } pmf.incr(comb(k, k2), prob * prob2)
    pmf
  }

  def +(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other, num.plus)
  def -(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other, num.minus)

  /**
   * Normalizes this PMF so the sum of all probabilities is 1.0.
   */
  def normalize() {
    val sum = hist.values.sum
    hist = hist.mapValues(_ / sum)
  }

  /**
   * Computes the mean of the PMF.
   * @return the mean of the PMF.
   */
  def mean(implicit num: Numeric[K]): Double =
    hist.map { case (h, prob) => num.toDouble(h) * prob }.sum

  def toCdf(implicit ord: Ordering[K]): Cdf[K] = Cdf(hist.toSeq)

  private[this] def pad(str: String, n: Int): String =
    if(str.length > n) str.substring(0, n) else str + (" " * (n - str.length))

  def print()(implicit ord: Ordering[K]) {
    if(hist.nonEmpty) {
      val keyLen = hist.keys.map(_.toString.length).max
      hist.toSeq.sortBy(_._1).map {
        case (h, prob) => pad(h.toString, keyLen) + " " + prob
      }.foreach(println)
    }
  }

  def printChart()(implicit ord: Ordering[K]) {
    if(hist.nonEmpty) {
      val keyLen = hist.keys.map(_.toString.length).max
      hist.toSeq.sortBy(_._1).map {
        case (h, prob) =>
          pad(h.toString, keyLen).mkString + " " +
            pad(prob.toString, 6) + " " +
            ("#" * (50 * prob).toInt)
      }.foreach(println)
    }
  }
}

object Pmf {

  def apply[K](probs: Map[K, Double]) = {
    val pmf = new Pmf[K]
    pmf.hist = probs
    pmf
  }

  def apply[K](probs: (K, Double)*) = {
    val pmf = new Pmf[K]
    probs.foreach { case (k, prob) => pmf.incr(k, prob) }
    pmf
  }

  def apply[K](probs: TraversableOnce[K]) = {
    val pmf = new Pmf[K]
    probs.foreach(pmf.incr(_))
    pmf
  }
}
