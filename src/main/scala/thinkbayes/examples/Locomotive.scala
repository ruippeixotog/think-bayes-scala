package thinkbayes.examples

import thinkbayes.Suite
import thinkbayes.extensions.Plotting._
import thinkbayes.extensions.Stats._

class Locomotive(hypos: Seq[Int], alpha: Double = 0.0) extends Suite[Int, Int] {
  hypos.foreach { hypo => set(hypo, math.pow(hypo, -alpha)) }
  normalize()

  def likelihood(data: Int, hypo: Int) =
    if(hypo < data) 0 else 1.0 / hypo
}

/**
 * Application for solving the locomotive problem (page 20):
 *
 * "A railroad numbers its locomotives in order 1..N. One day you see a locomotive with
 * the number 60. Estimate how many locomotives the railroad has."
 */
object LocomotiveApp extends App {
  val suite = new Locomotive(1 to 1000)
  val suite2 = new Locomotive(1 to 1000, 1.0)

  println("Plotting priors...")
  val priorPlot = suite.plotXY("Uniform", title = "Prior", xLabel = "Number of trains")
  suite2.plotXYOn(priorPlot)("Power law")

  println("Plotting posteriors after a train with number 60 is seen...")
  suite.update(60)
  suite2.update(60)
  val postPlot = suite.plotXY("Uniform", title = "After train #60", xLabel = "Number of trains")
  suite2.plotXYOn(postPlot)("Power law")

  println("Mean of the distribution after #60 is seen:")
  println("Uniform prior: " + suite.mean)
  println("Power law prior: " + suite2.mean)

  println("90% credible interval after #60 is seen:")
  println("Uniform prior: " + suite.credibleInterval(0.9))
  println("Power law prior: " + suite2.credibleInterval(0.9))
}
