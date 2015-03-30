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
    val pmf = normalPmf(2.7, 0.3, steps = 100)

    def likelihood(k: Int, lam: Double) = poissonPmf(lam).prob(k)
  }

  def numGoalsPmf(goalsPerGamePmf: Pmf[Double]): Pmf[Int] =
    goalsPerGamePmf.mapKeys(poissonPmf).mixture

  def goalTimePmf(goalsPerGamePmf: Pmf[Double]): Pmf[Double] =
    goalsPerGamePmf.mapKeys(exponentialPmf(_, absCutoff = 2.0)).mixture

  // ---------

  println("Plotting the posterior distribution of the average number of goals per game...")
  val perGameChartTitle = "Average number of goals per game"

  val bruinsPosterior = Hockey.observed(0, 2, 8, 4)
  val canucksPosterior = Hockey.observed(1, 3, 1, 0)

  val perGameChart = bruinsPosterior.plotXY("Bruins", title = perGameChartTitle, xLabel = "Goals per game")
  canucksPosterior.plotXYOn(perGameChart, "Canucks")

  println("Plotting the distribution of goals in a single game...")
  val goalsChartTitle = "Goals in a single game"

  val bruinsGoalsPmf = numGoalsPmf(bruinsPosterior.pmf)
  val canucksGoalsPmf = numGoalsPmf(canucksPosterior.pmf)

  val goalsChart = bruinsGoalsPmf.plotXY("Bruins", title = goalsChartTitle, xLabel = "Goals")
  canucksGoalsPmf.plotXYOn(goalsChart, "Canucks")

  println()
  println("Outcome at the end of regulation play:")

  val goalDiffPmf = bruinsGoalsPmf -- canucksGoalsPmf

  println("Win: %.2f%%".format(goalDiffPmf.prob(_ > 0) * 100.0))
  println("Tie: %.2f%%".format(goalDiffPmf.prob(0) * 100.0))
  println("Lose: %.2f%%".format(goalDiffPmf.prob(_ < 0) * 100.0))

  println()
  println("Plotting the distribution of the time between goals...")
  val goalTimeChartTitle = "Time between goals"

  val bruinsGoalTimePmf = goalTimePmf(bruinsPosterior.pmf)
  val canucksGoalTimePmf = goalTimePmf(canucksPosterior.pmf)

  val goalTimeChart = bruinsGoalTimePmf.plotXY("Bruins", title = goalTimeChartTitle, xLabel = "Games until goal")
  canucksGoalTimePmf.plotXYOn(goalTimeChart, "Canucks")

  println()
  println("Outcome if an overtime occurs:")

  val timeDiffPmf = bruinsGoalTimePmf -- canucksGoalTimePmf

  println("Win: %.2f%%".format(timeDiffPmf.prob(_ < 0) * 100.0))
  println("Lose: %.2f%%".format(timeDiffPmf.prob(_ > 0) * 100.0))

  println()

  val probWin = goalDiffPmf.prob(_ > 0) + goalDiffPmf.prob(0) * timeDiffPmf.prob(_ < 0)
  println("Overall probability of winning the next game: %.2f%%".format(probWin * 100.0))

  val probWinSeries = probWin * probWin + 2 * probWin * (1 - probWin) * probWin
  println("Overall probability of winning the series: %.2f%%".format(probWinSeries * 100.0))
}
