package thinkbayes

import scala.collection.generic.CanBuildFrom

trait Pmf[K] extends Map[K, Double] with PmfLike[K, Pmf[K]] {
  override def empty: Pmf[K] = Pmf.empty[K]
  override def seq: Pmf[K] = this
}

object Pmf extends PmfFactory[Pmf] {
  def empty[K]: Pmf[K] = HistogramPmf.empty[K]

  override def apply[K](probs: (K, Double)*): Pmf[K] = probs.foldLeft(Pmf.empty[K])(_ + _).normalized

  def apply[K](hist: Map[K, Double]): Pmf[K] = HistogramPmf(hist)

  def apply[K](keys: TraversableOnce[K]): Pmf[K] =
    keys.foldLeft(Pmf.empty[K]) { case (acc, k) => acc + (k -> 1.0) }.normalized

  implicit def canBuildFrom[K]: CanBuildFrom[Coll, (K, Double), Pmf[K]] = new PmfCanBuildFrom[K]
}
