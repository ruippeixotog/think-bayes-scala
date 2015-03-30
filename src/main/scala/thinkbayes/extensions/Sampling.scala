package thinkbayes.extensions

import thinkbayes.Pmf

object Sampling {

  def randomJoin[K, J](pmfs: TraversableOnce[Pmf[K]], join: TraversableOnce[K] => J): J =
    join(pmfs.map(_.sample()))

  def sampleJoin[K, J](pmfs: TraversableOnce[Pmf[K]], n: Int, join: TraversableOnce[K] => J) =
    Pmf(Seq.fill(n)(randomJoin(pmfs, join)))

  def randomSum[K: Numeric](pmfs: TraversableOnce[Pmf[K]]) = randomJoin[K, K](pmfs, _.sum)
  def sampleSum[K: Numeric](pmfs: TraversableOnce[Pmf[K]], n: Int) = sampleJoin[K, K](pmfs, n, _.sum)
  def randomMax[K: Ordering](pmfs: TraversableOnce[Pmf[K]]): K = randomJoin[K, K](pmfs, _.max)
  def sampleMax[K: Ordering](pmfs: TraversableOnce[Pmf[K]], n: Int) = sampleJoin[K, K](pmfs, n, _.max)
}
