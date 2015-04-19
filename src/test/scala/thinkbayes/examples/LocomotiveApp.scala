package thinkbayes.examples

import thinkbayes._
import thinkbayes.extensions.Plotting._
import thinkbayes.extensions.Stats._

/**
 * Application for solving the locomotive problem (page 20):
 *
 * "A railroad numbers its locomotives in order 1..N. One day you see a locomotive with
 * the number 60. Estimate how many locomotives the railroad has."
 */
object LocomotiveApp extends App {

  case class Locomotive(hypos: Seq[Int], alpha: Double = 0.0) extends SimpleSuite[Int, Int] {
    val pmf = Pmf(hypos.map { hypo => (hypo, math.pow(hypo, -alpha)) }.toMap).normalized

    def likelihood(data: Int, hypo: Int) =
      if (hypo < data) 0 else 1.0 / hypo
  }

  // ---------

  val prior = Locomotive(1 to 1000)
  val prior2 = Locomotive(1 to 1000, 1.0)

  println("Plotting priors...")
  val priorPlot = prior.plotXY("Uniform", title = "Prior", xLabel = "Number of trains")
  prior2.plotXYOn(priorPlot, "Power law")

  println()
  println("Plotting posteriors after a train with number 60 is seen...")
  val posterior = prior.observed(60)
  val posterior2 = prior2.observed(60)
  val postPlot = posterior.plotXY("Uniform", title = "After train #60", xLabel = "Number of trains")
  posterior2.plotXYOn(postPlot, "Power law")

  println()
  println("Mean of the distribution after #60 is seen:")
  println("Uniform prior: " + posterior.pmf.mean)
  println("Power law prior: " + posterior2.pmf.mean)

  println()
  println("90% credible interval after #60 is seen:")
  println("Uniform prior: " + posterior.pmf.credibleInterval(0.9))
  println("Power law prior: " + posterior2.pmf.credibleInterval(0.9))

  println()
  println("Mean of the distribution after #30 and #90 are seen after #60:")
  val posterior3 = posterior.observed(60, 90)
  val posterior4 = posterior2.observed(60, 90)
  println("Uniform prior: " + posterior3.pmf.mean)
  println("Power law prior: " + posterior4.pmf.mean)

  println()
  println("90% credible interval after #30 and #90 are seen after #60:")
  println("Uniform prior: " + posterior3.pmf.credibleInterval(0.9))
  println("Power law prior: " + posterior4.pmf.credibleInterval(0.9))
}
