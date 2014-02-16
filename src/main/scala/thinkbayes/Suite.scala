package thinkbayes

abstract class Suite[H, D] extends Pmf[H] {

  /**
   * Computes the likelihood of a given data under an hypothesis.
   * @param data the representation of the data whose likelihood is to be returned
   * @param hypo the representation of the hypothesis
   * @return the likelihood of the given data under the given hypothesis.
   */
  def likelihood(data: D, hypo: H): Double

  /**
   * Updates each hypothesis based on the given data.
   * Modifies the suite directly; if you want to keep the original, make a copy.
   * @param data the representation of the data to use to update the suite
   */
  def update(data: D) {
    hist = hist.map { case (h, prob) => (h, prob * likelihood(data, h)) }
    normalize()
  }

  /**
   * Updates each hypothesis based on the dataset.
   * This is more efficient than calling `update` repeatedly because it waits until the end to
   * `normalize`.
   * Modifies the suite directly; if you want to keep the original, make a copy.
   * @param dataset a sequence of data values to use to update the suite
   */
  def updateSet(dataset: TraversableOnce[D]) {
    dataset.foreach { data =>
      hist = hist.map { case (h, prob) => (h, prob * likelihood(data, h)) }
    }
    normalize()
  }
}
