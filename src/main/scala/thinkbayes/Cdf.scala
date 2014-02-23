package thinkbayes

class Cdf[K: Ordering] {
  var vals = IndexedSeq.empty[(K, Double)]

  def prob(key: K): Double = searchBy[(K, Double), K](vals, key, _._1) match {
    case Left((_, p)) => p
    case Right(_) => throw new NoSuchElementException
  }

  def value(prob: Double): K = searchBy[(K, Double), Double](vals, prob, _._2) match {
    case Left((key, _)) => key
    case Right(nextIdx) if nextIdx == vals.length => vals.last._1
    case Right(nextIdx) => vals(nextIdx)._1
  }

  def pow(p: Double)(implicit num: Numeric[K]): Cdf[K] = {
    val cdf = new Cdf[K]
    cdf.vals = vals.map { case (k, prob) => (k, math.pow(prob, p)) }
    cdf
  }

  def toPmf: Pmf[K] = {
    val pmf = new Pmf[K]
    vals.foldLeft(0.0) { case (last, (k, prob)) =>
      pmf.set(k, prob - last)
      prob
    }
    pmf
  }

  private[this] def searchBy[A, V <% Ordered[V]](xs: IndexedSeq[A], target: V, f: A => V): Either[A, Int] = {
    def bs(target: V, start: Int, end: Int): Either[A, Int] = {
      if (start == end) {
        if(start == xs.length || f(xs(start)) > target) Right(start)
        else Right(start + 1)
      } else {
        val mid = (end + start) / 2
        val midVal = f(xs(mid))

        if (midVal == target) Left(xs(mid))
        else if(midVal > target) bs(target, start, mid)
        else bs(target, mid + 1, end)
      }
    }
    bs(target, 0, xs.length)
  }
}

object Cdf {

  def apply[K: Ordering](values: Seq[(K, Double)]): Cdf[K] = {
    val cdf = new Cdf[K]
    var total = 0.0
    cdf.vals = values.sorted.map { case (key, prob) =>
      total += prob
      (key, total)
    }.toIndexedSeq

    cdf.vals = cdf.vals.map { case (key, prob) => (key, prob / total) }
    cdf
  }

  def apply[K: Ordering](values: TraversableOnce[K]) = Pmf(values).toCdf
}
