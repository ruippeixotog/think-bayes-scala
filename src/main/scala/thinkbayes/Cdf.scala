package thinkbayes

trait Cdf[K] {
  def iterator: Iterator[(K, Double)]
  def prob(key: K): Double
  def value(prob: Double): K

  def pow(p: Double)(implicit num: Numeric[K]): Cdf[K] =
    Cdf(iterator.map { case (k, prob) => (k, math.pow(prob, p)) }.toSeq: _*)

  def toPmf: Pmf[K] = {
    val b = Pmf.newBuilder[K]
    var last = 0.0
    for ((k, prob) <- iterator) { b += (k -> (prob - last)); last = prob }
    b.result()
  }
}

object Cdf {
  def apply[K: Ordering](values: (K, Double)*): Cdf[K] = CategoricalCdf(values: _*)
  def apply[K: Ordering](keys: TraversableOnce[K]): Cdf[K] = Pmf(keys).toCdf
}
