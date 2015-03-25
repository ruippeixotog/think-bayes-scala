package thinkbayes

import thinkbayes.PmfFactory.PmfBuilder

import scala.collection.MapLike
import scala.util.Random

trait PmfLike[K, +This <: PmfLike[K, This] with Pmf[K]] extends MapLike[K, Double, This] {

  protected[this] override def newBuilder = new PmfBuilder(empty)

  // overloaded methods to deal with the fact that a Pmf has a fixed value type
  def +(kv: (K, Double))(implicit dummy: DummyImplicit): Pmf[K]

  def +[B1 >: Double](kv: (K, B1)): Map[K, B1] = Map() ++ iterator + kv

  /**
   * Gets the probability associated with a given key.
   * @param key the key whose probability is to be returned
   * @return the probability associated with the given key.
   */
  def prob(key: K): Double = getOrElse(key, 0.0)

  def prob(pred: K => Boolean): Double = iterator.filter { case (k, _) => pred(k) }.map(_._2).sum

  /**
   * Returns the highest probability with its associated value.
   * @return the highest probability with its associated value.
   */
  def maxProb: (K, Double) = maxBy(_._2)

  def mode: K = maxBy(_._2)._1

  /**
   * Computes the mean of the PMF.
   * @return the mean of the PMF.
   */
  def mean(implicit num: Numeric[K]): Double =
    iterator.map { case (h, prob) => num.toDouble(h) * prob }.sum

  def random(): K = {
    def get(rand: Double, it: Iterator[(K, Double)]): K = {
      val (k, prob) = it.next()
      if (rand < prob) k else get(rand - prob, it)
    }
    get(Random.nextDouble() * values.sum, iterator)
  }

  def mapKeys[K2](f: K => K2): Pmf[K2] = map { case (k, prob) => (f(k), prob) }.normalized

  def mapValues(f: Double => Double)(implicit dummy: DummyImplicit): Pmf[K] = {
    val b = newBuilder
    b ++= iterator.map { kv => (kv._1, f(kv._2)) }
    b.result()
  }

  override def filterKeys(p: K => Boolean): Pmf[K] = filter { kv => p(kv._1) }

  /**
   * Normalizes this PMF so the sum of all probabilities is 1.0.
   */
  def normalized: Pmf[K] = {
    val sum = values.sum
    if (sum == 0) this.asInstanceOf[Pmf[K]] else mapValues { prob: Double => prob / sum }
  }

  def join[J](other: Pmf[K], comb: (K, K) => J): Pmf[J] = {
    foldLeft(Pmf.empty[J]) {
      case (acc, (k, prob)) =>
        other.foldLeft(acc) { case (acc2, (k2, prob2)) => acc2 + (comb(k, k2), prob * prob2) }
    }.normalized
  }

  def ++(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other, num.plus)
  def --(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other, num.minus)

  def mixture[K2](implicit ev: K <:< Pmf[K2]): Pmf[K2] = {
    foldLeft(Pmf.empty[K2]) {
      case (acc, (outcome, weight)) =>
        outcome.foldLeft(acc) { case (acc2, (k, prob)) => acc2 + (k, weight * prob) }
    }.normalized
  }

  def toCdf(implicit ord: Ordering[K]): Cdf[K] = Cdf(toSeq: _*)

  override def stringPrefix = "Pmf"
}
