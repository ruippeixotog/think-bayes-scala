package thinkbayes

import thinkbayes.PmfFactory.PmfBuilder

import scala.collection.MapLike
import scala.collection.generic.CanBuildFrom
import scala.util.Random

trait PmfLike[K, +This <: PmfLike[K, This] with Pmf[K]] extends MapLike[K, Double, This] {

  protected[this] override def newBuilder = new PmfBuilder(empty)

  /**
    * Adds a probability value to an outcome of this distribution, returning a new `Pmf`.
    *
    * This method can either cause the creation of a new outcome with the given probability or add the given probability
    * to an existing outcome. Either way, this method does _not_ normalize the `Pmf` before returning it - it is the
    * user's responsibility to do so after this operation. That way, the probability of several outcomes can be
    * increased, followed by a single normalization.
    *
    * @param kv the outcome-probability pair to add
    * @return a new `Pmf` with the added outcome-probability pair.
    */
  def +(kv: (K, Double))(implicit dummy: DummyImplicit): Pmf[K]

  def +[B1 >: Double](kv: (K, B1)): Map[K, B1] = Map() ++ iterator + kv

  /**
    * Returns the probability that an outcome occurs.
    * @param key the outcome whose probability is to be returned
    * @return the probability that the given outcome occurs.
    */
  def prob(key: K): Double = getOrElse(key, 0.0)

  /**
    * Returns the probability that an outcome that satisfies a given predicate occurs.
    * @param pred the predicate used to test outcomes
    * @return the probability that an outcome that satisfies the predicate `pred` occurs.
    */
  def prob(pred: K => Boolean): Double = iterator.filter { case (k, _) => pred(k) }.map(_._2).sum

  /**
    * Returns the mode of this distribution along with its probability.
    * @return a pair containing the mode of this distribution and its probability.
    */
  def maxProb: (K, Double) = maxBy(_._2)

  /**
    * Returns the mode of this distribution.
    * @return the mode of this distribution.
    */
  def mode: K = maxBy(_._2)._1

  /**
    * Returns the mean of this distribution.
    * @return the mean of this distribution.
    */
  def mean(implicit num: Numeric[K]): Double =
    iterator.map { case (h, prob) => num.toDouble(h) * prob }.sum

  /**
    * Returns the variance of this distribution.
    * @return the variance of this distribution.
    */
  def variance(implicit num: Numeric[K]): Double = {
    val m = mean
    iterator.map { case (h, prob) => num.toDouble(h) * num.toDouble(h) * prob }.sum - (m * m)
  }

  /**
    * Generates a random sample of this distribution.
    * @return a random sample of this distribution.
    */
  def sample(): K = {
    def get(rand: Double, it: Iterator[(K, Double)]): K = {
      val (k, prob) = it.next()
      if (rand < prob) k else get(rand - prob, it)
    }
    get(Random.nextDouble() * values.sum, iterator)
  }

  /**
    * Transforms this `Pmf` by applying a function to every outcome. If the function maps two different outcomes to the
    * same value, their probabilities are combined.
    *
    * @param f the function used to transform the outcomes of this `Pmf`
    * @tparam K2 the type of the resulting outcomes
    * @return a new `Pmf` with every outcome of this `Pmf` transformed.
    */
  def mapKeys[K2, That](f: K => K2)(implicit bf: CanBuildFrom[This, (K2, Double), That]): That =
    map { case (k, prob) => (f(k), prob) }

  /**
    * Transforms this `Pmf` by applying a function to every probability. This method does _not_ normalize the `Pmf`
    * before returning it.
    *
    * @param f the function used to transform the outcomes of this `Pmf`
    * @return a new `Pmf` with the probability of every outcome of this `Pmf` transformed.
    */
  def mapValues(f: Double => Double)(implicit dummy: DummyImplicit): This = {
    val b = newBuilder
    b ++= iterator.map { kv => (kv._1, f(kv._2)) }
    b.result()
  }

  override def filterKeys(p: K => Boolean): This = filter { kv => p(kv._1) }

  /**
    * Normalizes this `Pmf` so the probabilities of all outcomes sum to 1.0.
    * @return a new `Pmf` with its probabilities normalized.
    */
  def normalized: This = {
    val sum = values.sum
    if (sum == 0.0 || sum == 1.0) repr else mapValues { prob: Double => prob / sum }
  }

  /**
    * Joins two independent `Pmf`s by combining their outcomes using a function.
    *
    * Each outcome from this `Pmf` is combined with each outcome from `other`, and their joint probability is calculated.
    * If the function maps two different outcome pairs to the same value, their probabilities are combined. This method
    * always returns a normalized `Pmf`.
    *
    * @param other the `Pmf` to combine with this `Pmf`
    * @param comb the function used to combine outcomes from the two `Pmf`s
    * @tparam K2 the type of the outcomes of `other`
    * @tparam J the type of the resulting outcomes
    * @return a new `Pmf` resultant from combining this `Pmf` and `other` using the function `comb`
    */
  def join[K2, J, That](other: Pmf[K2])(comb: (K, K2) => J): Pmf[J] = {
    val b = Pmf.newBuilder[J]
    for ((k, prob) <- this; (k2, prob2) <- other)
      b += (comb(k, k2) -> prob * prob2)
    b.result().normalized
  }

  /**
    * Returns the distribution of the sum of two independent `Pmf`s. This method always returns a normalized `Pmf`.
    *
    * @param other the `Pmf` to sum with this `Pmf`
    * @param num an evidence that the outcomes of this distribution are numeric
    * @return the distribution of the sum of this `Pmf` with `other`.
    */
  def ++(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other)(num.plus)

  /**
    * Returns the distribution of the difference of two independent `Pmf`s. This method always returns a normalized
    * `Pmf`.
    *
    * @param other the `Pmf` to subtract to this `Pmf`
    * @param num an evidence that the outcomes of this distribution are numeric
    * @return the distribution of the difference between this `Pmf` and `other`.
    */
  def --(other: Pmf[K])(implicit num: Numeric[K]): Pmf[K] = join(other)(num.minus)

  /**
    * Returns the mixture distribution that results from the combination of the `Pmf` outcomes. This method always
    * returns a normalized `Pmf`.
    *
    * @param ev an evidence that the outcomes of this distribution are also `Pmf`s
    * @tparam K2 the type of the outcomes in each outcome `Pmf`
    * @return the mixture distribution that results from the combination of the `Pmf` outcomes.
    */
  def mixture[K2](implicit ev: K <:< Pmf[K2]): Pmf[K2] = {
    val b = Pmf.newBuilder[K2]
    for ((outcome, weight) <- this; (k, prob) <- outcome)
      b += (k -> weight * prob)
    b.result().normalized
  }

  /**
    * Returns a copy of this `Pmf` that is an instance of `CategoricalPmf`. This method can be used to force the
    * calculation of the probabilities of lazy `Pmf` instances or to convert `Pmf`s whose probabilities are described as
    * a closed-form expression into an enumeration of all outcomes and its probabilities (a
    * [[http://en.wikipedia.org/wiki/Categorical_distribution categorical distribution]]).
    *
    * @return a copy of this `Pmf` that is an instance of `CategoricalPmf`.
    */
  def toCategoricalPmf: CategoricalPmf[K] = CategoricalPmf(iterator.toSeq: _*)

  /**
    * Returns a `Cdf` that represents this distribution.
    * @param ord an evidence that the outcomes of this distribution have an ordering
    * @return a `Cdf` that represents this distribution.
    */
  def toCdf(implicit ord: Ordering[K]): Cdf[K] = Cdf(toSeq: _*)

  override def stringPrefix = "Pmf"
}
