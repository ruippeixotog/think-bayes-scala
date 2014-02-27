package thinkbayes.examples

import thinkbayes._
import thinkbayes.extensions.Distributions._
import thinkbayes.extensions.Plotting._

object HockeyApp extends App {

  object Hockey extends Suite[Double, Int] {
    val pmf = normalPmf(2.7, 0.3)

    def likelihood(k: Int, lam: Double) = poissionPmf(lam).prob(k)
  }

  // ---------

  val bruinsPosterior = Hockey.observed(0, 2, 8, 4)
  val canucksPosterior = Hockey.observed(1, 3, 1, 0)

  val chart = bruinsPosterior.plotXY("Bruins", title = "Hockey", xLabel = "Goals")
  canucksPosterior.plotXYOn(chart, "Canucks")
}
