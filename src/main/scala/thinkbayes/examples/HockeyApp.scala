package thinkbayes.examples

import thinkbayes._
import thinkbayes.extensions.Distributions._
import thinkbayes.extensions.Plotting._

/**
 * Application for studying the The Boston Bruins problem (page 65):
 *
 * "In the 2010-11 National Hockey League (NHL) Finals, my beloved Boston Bruins played
 * a best-of-seven championship series against the despised Vancouver Canucks. Boston
 * lost the first two games 0-1 and 2-3, then won the next two games 8-1 and 4-0. At this
 * point in the series, what is the probability that Boston will win the next game, and what
 * is their probability of winning the championship?"
 */
object HockeyApp extends App {

  object Hockey extends Suite[Double, Int] {
    val pmf = normalPmf(2.7, 0.3)

    def likelihood(k: Int, lam: Double) = poissionPmf(lam).prob(k)
  }

  def goalPmf(goalsPerGamePmf: Pmf[Double]): Pmf[Int] = {
    val probs = goalsPerGamePmf.hist.toSeq.map { case (lam, prob) => (poissionPmf(lam), prob) }
    Pmf(probs: _*).mixture
  }

  // ---------

  println("Plotting the posterior distribution of the average number of goals per game...")
  val perGameChartTitle = "Average number of goals per game"

  val bruinsPosterior = Hockey.observed(0, 2, 8, 4)
  val canucksPosterior = Hockey.observed(1, 3, 1, 0)

  val perGameChart = bruinsPosterior.plotXY("Bruins", title = perGameChartTitle, xLabel = "Goals per game")
  canucksPosterior.plotXYOn(perGameChart, "Canucks")

  println("Plotting the distribution of goals in a single game...")
  val goalsChartTitle = "Goals in a single game"

  val bruinsGoalPmf = goalPmf(bruinsPosterior.pmf)
  val canucksGoalPmf = goalPmf(canucksPosterior.pmf)

  val goalsChart = bruinsGoalPmf.plotXY("Bruins", title = goalsChartTitle, xLabel = "Goals")
  canucksGoalPmf.plotXYOn(goalsChart, "Canucks")

  println()
  println("Outcome probabilities:")

  val goalDiffCdf = bruinsGoalPmf - canucksGoalPmf

  println("Win: %.2f%%".format(goalDiffCdf.prob(_ > 0) * 100.0))
  println("Tie: %.2f%%".format(goalDiffCdf.prob(0) * 100.0))
  println("Lose: %.2f%%".format(goalDiffCdf.prob(_ < 0) * 100.0))
}
