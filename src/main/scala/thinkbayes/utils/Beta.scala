package thinkbayes.utils

import scala.math._
import thinkbayes.Pdf

class Beta(var alpha: Double = 1.0, var beta: Double = 1.0) extends Pdf {
  override def density(x: Double) = pow(x, alpha - 1) * pow(1 - x, beta - 1)

  def update(data: Boolean) {
    if(data) alpha += 1.0 else beta += 1.0
  }

  def updateSet(dataset: TraversableOnce[Boolean]) {
    val (trues, falses) = dataset.foldLeft((0, 0)) {
      case ((t, f), data) => if(data) (t + 1, f) else (t, f + 1)
    }
    alpha += trues
    beta += falses
  }

  def updateSet(dataCounts: (Int, Int)) {
    alpha += dataCounts._1
    beta += dataCounts._2
  }

  def mean = alpha / (alpha + beta)
}
