package thinkbayes

trait Pdf {

  def density(x: Double): Double

  def toPmf(values: TraversableOnce[Double]): Pmf[Double] = {
    val pmf = new Pmf[Double]
    for(k <- values) pmf.set(k, density(k))
    pmf.normalize()
    pmf
  }
}

trait BoundedPdf extends Pdf {
  def lowerBound: Double
  def upperBound: Double

  def toPmf(step: Double = (upperBound - lowerBound) / 10000): Pmf[Double] =
    toPmf(lowerBound to upperBound by step)
}
