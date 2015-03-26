package thinkbayes

trait Suite[H, D] {

  def pmf: Pmf[H]

  /**
   * Computes the likelihood of a given data under an hypothesis.
   * @param data the representation of the data whose likelihood is to be returned
   * @param hypo the representation of the hypothesis
   * @return the likelihood of the given data under the given hypothesis.
   */
  def likelihood(data: D, hypo: H): Double

  /**
   * Updates each hypothesis based on the given data.
   * @param data the representation of the data to use to update the suite
   * @return a new [[Suite]] with the updated hypotheses.
   */
  def observed(data: D): Suite[H, D] = {
    val newPmf = pmf.map { case (h, prob) => (h, prob * likelihood(data, h)) }.normalized
    updatedPmf(newPmf)
  }

  def observed(dataset: D*): Suite[H, D] = observedSet(dataset)

  /**
   * Updates each hypothesis based on the given dataset.
   * This is more efficient than calling `update` repeatedly because it waits until the end to
   * `normalize`.
   * @param dataset a sequence of data values to use to update the suite
   * @return a new [[Suite]] with the updated hypotheses.
   */
  def observedSet(dataset: TraversableOnce[D]): Suite[H, D] = {
    val newPmf = dataset.foldLeft(pmf) { (acc, data) =>
      acc.map { case (h, prob) => (h, prob * likelihood(data, h)) }
    }.normalized

    updatedPmf(newPmf)
  }

  /**
   * Returns a new `Suite` with an updated `Pmf`.
   * @param newPmf the `Pmf` of the `Suite` to be returned
   * @return a new `Suite` with an updated `Pmf`.
   */
  def updatedPmf(newPmf: Pmf[H]): Suite[H, D] = Suite(newPmf)(likelihood)
}

object Suite {

  def apply[H, D](distr: Pmf[H])(likelihoodFunc: (D, H) => Double): Suite[H, D] = new Suite[H, D] {
    val pmf = distr
    def likelihood(data: D, hypo: H) = likelihoodFunc(data, hypo)
    override def updatedPmf(newPmf: Pmf[H]) = Suite(newPmf)(likelihoodFunc)
  }
}
