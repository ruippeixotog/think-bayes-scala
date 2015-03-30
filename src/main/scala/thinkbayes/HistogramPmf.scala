package thinkbayes

case class HistogramPmf[K](hist: Map[K, Double]) extends Pmf[K] {

  def +(kv: (K, Double))(implicit dummy: DummyImplicit) =
    HistogramPmf(hist.updated(kv._1, hist.getOrElse(kv._1, 0.0) + kv._2))

  def -(key: K): Pmf[K] = HistogramPmf(hist - key)
  def iterator: Iterator[(K, Double)] = hist.iterator
  def get(key: K): Option[Double] = hist.get(key)

  override def mapValues(f: Double => Double)(implicit dummy: DummyImplicit) = HistogramPmf(hist.mapValues(f))
  override def +[B1 >: Double](kv: (K, B1)) = hist + kv

  override def toHistogramPmf: Pmf[K] = this
}

object HistogramPmf {
  def empty[K] = HistogramPmf(Map.empty[K, Double])
}
