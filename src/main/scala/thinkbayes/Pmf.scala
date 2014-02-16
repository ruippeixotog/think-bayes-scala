package thinkbayes

class Pmf[K] {
  var hist = Map[K, Double]()

  def prob(key: K) = hist.getOrElse(key, 0.0)

  def set(key: K, prob: Double) = hist += (key -> prob)
  def incr(key: K) = hist += (key -> (hist.getOrElse(key, 0.0) + 1))
  def mult(key: K, factor: Double) = hist += (key -> (hist.getOrElse(key, 0.0) * factor))

  def max: K = hist.maxBy(_._2)._1

  def normalize() {
    val sum = hist.values.sum
    hist = hist.mapValues(_ / sum)
  }

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
