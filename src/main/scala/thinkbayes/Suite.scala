package thinkbayes

abstract class Suite[H, D] extends Pmf[H] {
  def likelihood(data: D, hypo: H): Double

  def update(data: D) = {
    hist = hist.map { case (h, prob) => (h, prob * likelihood(data, h)) }
    normalize()
  }
}
