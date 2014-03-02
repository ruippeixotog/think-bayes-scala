package thinkbayes

trait Pdf {

  def density(x: Double): Double

  def bounded(lower: Double, upper: Double) = new BoundedPdf {
    def density(x: Double) = Pdf.this.density(x)
    def lowerBound = lower
    def upperBound = upper
  }

  def toPmf(values: TraversableOnce[Double]): Pmf[Double] =
    Pmf(values.map { k => (k, density(k)) }.toMap).normalized
}

trait BoundedPdf extends Pdf {
  def lowerBound: Double
  def upperBound: Double

  def toPmf(step: Double = (upperBound - lowerBound) / 2000): Pmf[Double] =
    toPmf(lowerBound to upperBound by step)
}

object Pdf {

  def apply(densityFunc: Double => Double): Pdf = new Pdf {
    def density(x: Double) = densityFunc(x)
  }

  def apply(lower: Double, upper: Double)(densityFunc: Double => Double) = new BoundedPdf {
    def density(x: Double) = densityFunc(x)
    val lowerBound = lower
    val upperBound = upper
  }
}
