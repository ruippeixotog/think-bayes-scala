package thinkbayes

trait SimpleSuite[H, D] extends Suite[H, D] {

  /**
    * Computes the likelihood of a given data under an hypothesis.
    * @param data the representation of the data whose likelihood is to be returned
    * @param hypo the representation of the hypothesis
    * @return the likelihood of the given data under the given hypothesis.
    */
  def likelihood(data: D, hypo: H): Double

  def observed(data: D): Suite[H, D] = {
    val newPmf = pmf.map { case (h, prob) => (h, prob * likelihood(data, h)) }.normalized
    updatedPmf(newPmf)
  }

  override def observedSet(dataset: TraversableOnce[D]): Suite[H, D] = {
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
