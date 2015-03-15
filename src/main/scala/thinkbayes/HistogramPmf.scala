package thinkbayes

case class HistogramPmf[K](hist: Map[K, Double]) extends Pmf[K] {

  def +[B1 >: Double](kv: (K, B1)): Map[K, B1] = hist + kv
  def -(key: K): Pmf[K] = HistogramPmf(hist - key)
  def iterator: Iterator[(K, Double)] = hist.iterator
  def get(key: K): Option[Double] = hist.get(key)

  def +(kv: (K, Double))(implicit dummy: DummyImplicit): Pmf[K] =
    HistogramPmf(hist.updated(kv._1, hist.getOrElse(kv._1, 0.0) + kv._2))

  def mapValues(f: Double => Double)(implicit dummy: DummyImplicit): Pmf[K] = HistogramPmf(hist.mapValues(f))
}

object HistogramPmf {
  def empty[K] = HistogramPmf(Map.empty[K, Double])
}
