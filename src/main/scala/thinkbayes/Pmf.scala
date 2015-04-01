package thinkbayes

import scala.collection.generic.CanBuildFrom

trait Pmf[K] extends Map[K, Double] with PmfLike[K, Pmf[K]] {
  override def empty: Pmf[K] = Pmf.empty[K]
  override def seq: Pmf[K] = this
}

object Pmf extends PmfFactory[Pmf] {
  def empty[K] = CategoricalPmf.empty[K]

  implicit def canBuildFrom[K]: CanBuildFrom[Coll, (K, Double), Pmf[K]] = new PmfCanBuildFrom[K]
}
