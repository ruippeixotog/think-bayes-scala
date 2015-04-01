package thinkbayes

import scala.collection.generic.CanBuildFrom

case class CategoricalPmf[K](ps: Map[K, Double]) extends Pmf[K] with PmfLike[K, CategoricalPmf[K]] {
  override def empty: CategoricalPmf[K] = CategoricalPmf.empty

  def +(kv: (K, Double))(implicit dummy: DummyImplicit) =
    CategoricalPmf(ps.updated(kv._1, ps.getOrElse(kv._1, 0.0) + kv._2))

  def -(key: K) = CategoricalPmf(ps - key)
  def iterator: Iterator[(K, Double)] = ps.iterator
  def get(key: K): Option[Double] = ps.get(key)

  override def mapValues(f: Double => Double)(implicit dummy: DummyImplicit) = CategoricalPmf(ps.mapValues(f))
  override def +[B1 >: Double](kv: (K, B1)) = ps + kv

  override def toCategoricalPmf = this
}

object CategoricalPmf extends PmfFactory[CategoricalPmf] {
  def empty[K] = CategoricalPmf(Map.empty[K, Double])

  implicit def canBuildFrom[K]: CanBuildFrom[Coll, (K, Double), CategoricalPmf[K]] = new PmfCanBuildFrom[K]
}
