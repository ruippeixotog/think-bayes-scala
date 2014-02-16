package thinkbayes

trait Pdf {

  def density(x: Double): Double

  def toPmf(values: Seq[Double]): Pmf[Double] = {
    val pmf = new Pmf[Double]
    values.foreach { v => pmf.set(v, density(v)) }
    pmf.normalize()
    pmf
  }
}

