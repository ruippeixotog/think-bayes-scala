package thinkbayes

import thinkbayes.PmfFactory.PmfBuilder

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

abstract class PmfFactory[CC[K] <: Pmf[K] with PmfLike[K, CC[K]]] {
  type Coll = CC[_]

  def apply[K](ps: (K, Double)*): CC[K] = (newBuilder[K] ++= ps).result()

  def apply[K](ps: Map[K, Double]): CC[K] = (newBuilder[K] ++= ps).result()

  def apply[K](keys: TraversableOnce[K]): CC[K] = (newBuilder[K] ++= keys.map(_ -> 1.0)).result().normalized

  def newBuilder[K]: mutable.Builder[(K, Double), CC[K]] = new PmfBuilder[K, CC[K]](empty[K])

  def empty[K]: CC[K]

  class PmfCanBuildFrom[K] extends CanBuildFrom[Coll, (K, Double), CC[K]] {
    def apply(from: Coll) = newBuilder[K]
    def apply() = newBuilder
  }
}

object PmfFactory {
  class PmfBuilder[K, Coll <: Pmf[K] with PmfLike[K, Coll]](empty: Coll) extends mutable.Builder[(K, Double), Coll] {
    var coll: Coll = empty

    def result() = coll
    def clear() { coll = empty }
    def +=(elem: (K, Double)) = {
      coll = (coll + elem).asInstanceOf[Coll]
      this
    }
  }
}
