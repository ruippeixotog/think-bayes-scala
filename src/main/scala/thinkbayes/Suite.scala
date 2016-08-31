package thinkbayes

trait Suite[H, D] {

  def pmf: Pmf[H]

  /**
    * Updates each hypothesis based on the given data.
    * @param data the representation of the data to use to update the suite
    * @return a new [[Suite]] with the updated hypotheses.
    */
  def observed(data: D): Suite[H, D]

  def observed(dataset: D*): Suite[H, D] = observedSet(dataset)

  /**
    * Updates each hypothesis based on the given dataset.
    * This is more efficient than calling `update` repeatedly because it waits until the end to
    * `normalize`.
    * @param dataset a sequence of data values to use to update the suite
    * @return a new [[Suite]] with the updated hypotheses.
    */
  def observedSet(dataset: TraversableOnce[D]): Suite[H, D] = dataset.foldLeft(this)(_.observed(_))
}

object Suite {

  def apply[H, D](distr: Pmf[H])(likelihoodFunc: (D, H) => Double): Suite[H, D] = new SimpleSuite[H, D] {
    val pmf = distr
    def likelihood(data: D, hypo: H) = likelihoodFunc(data, hypo)
    override def updatedPmf(newPmf: Pmf[H]) = Suite(newPmf)(likelihoodFunc)
  }
}
