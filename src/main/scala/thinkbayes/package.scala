
package object thinkbayes {

  implicit class ProbabilityMapUtils[K](hist: Map[K, Double]) {

    private[this] def pad(str: String, n: Int): String =
      if (str.length > n) str.substring(0, n) else str + (" " * (n - str.length))

    def toPmf = Pmf(hist)

    def print()(implicit ord: Ordering[K]) {
      if (hist.nonEmpty) {
        val keyLen = hist.keys.map(_.toString.length).max
        hist.toSeq.sortBy(_._1).map {
          case (h, prob) => pad(h.toString, keyLen) + " " + prob
        }.foreach(println)
      }
    }

    def printChart()(implicit ord: Ordering[K]) {
      if (hist.nonEmpty) {
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

  implicit def cdfProbMapUtils[K](cdf: Cdf[K]) = new ProbabilityMapUtils(cdf.iterator.toMap)
  implicit def suiteProbMapUtils[H](suite: Suite[H, _]) = new ProbabilityMapUtils(suite.pmf)
}
