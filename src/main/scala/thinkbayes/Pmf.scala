package thinkbayes

import scala.collection.MapLike
import scala.util.Random

case class Pmf[K](hist: Map[K, Double]) extends Map[K, Double] with MapLike[K, Double, Pmf[K]] {

  // methods to implement for the Map and MapLike interfaces
  def +[B1 >: Double](kv: (K, B1)): Map[K, B1] = hist + kv
  def -(key: K): Pmf[K] = Pmf(hist - key)
  def iterator: Iterator[(K, Double)] = hist.iterator
  def get(key: K): Option[Double] = hist.get(key)
  override def empty: Pmf[K] = Pmf.empty
  protected[this] override def newBuilder = Pmf.builder

  // overloaded methods to deal with the fact that a Pmf has a fixed value type
  def +(kv: (K, Double))(implicit dummy: DummyImplicit): Pmf[K] =
    Pmf(hist.updated(kv._1, hist.getOrElse(kv._1, 0.0) + kv._2))

  def mapValues(f: Double => Double)(implicit dummy: DummyImplicit): Pmf[K] = Pmf(hist.mapValues(f))

  // overriden methods where the Map interface does not consider `This` type
  override def filterKeys(p: (K) => Boolean): Pmf[K] = filter { kv => p(kv._1) }

  /**
   * Gets the probability associated with a given key.
   * @param key the key whose probability is to be returned
   * @return the probability associated with the given key.
   */
  def prob(key: K) = getOrElse(key, 0.0)

  def prob(pred: K => Boolean) = iterator.filter { case (k, _) => pred(k) }.map(_._2).sum

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

  /**
   * Normalizes this PMF so the sum of all probabilities is 1.0.
   */
  def normalized = {
    val sum = values.sum
    if (sum == 0) this else mapValues { prob: Double => prob / sum }
  }

  def mapKeys[K2](f: K => K2): Pmf[K2] = map { case (k, prob) => (f(k), prob) }.normalized

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
}

object Pmf {
  import scala.collection.generic.CanBuildFrom
  import scala.collection.mutable

  def builder[K] = new mutable.Builder[(K, Double), Pmf[K]] {
    var hist = Map.empty[K, Double]

    def result() = Pmf(hist)
    def clear() { hist = Map.empty }
    def +=(elem: (K, Double)) = {
      hist = hist.updated(elem._1, hist.getOrElse(elem._1, 0.0) + elem._2)
      this
    }
  }

  implicit def canBuildFrom[K]: CanBuildFrom[Pmf[_], (K, Double), Pmf[K]] =
    new CanBuildFrom[Pmf[_], (K, Double), Pmf[K]] {
      def apply() = builder[K]
      def apply(from: Pmf[_]) = builder[K]
    }

  def empty[K] = Pmf(Map.empty[K, Double])

  def apply[K](probs: (K, Double)*): Pmf[K] = probs.foldLeft(Pmf.empty[K])(_ + _).normalized

  def apply[K](keys: TraversableOnce[K]): Pmf[K] =
    keys.foldLeft(Pmf.empty[K]) { case (acc, k) => acc + (k -> 1.0) }.normalized
}
