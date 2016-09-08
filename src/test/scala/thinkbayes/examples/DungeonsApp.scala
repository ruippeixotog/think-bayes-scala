package thinkbayes.examples

import thinkbayes.Pmf
import thinkbayes.extensions.Plotting._
import thinkbayes.extensions.Sampling._

/**
  * Application for solving the Dungeons and Dragons d6 problem (page 42):
  *
  * "The first example is based on Dungeons & Dragons, a role-playing game where the results
  * of players’ decisions are usually determined by rolling dice. In fact, before game play
  * starts, players generate each attribute of their characters—strength, intelligence, wisdom,
  * dexterity, constitution, and charisma—by rolling three 6-sided dice and adding
  * them up.
  *
  * So you might be curious to know the distribution of this sum."
  */
object DungeonsApp extends App {

  def die(sides: Int) = Pmf(1 to sides)

  // ---------

  // sum and maxima
  val three = Seq.fill(3)(die(6))

  val threeSum = sampleSum(three, 1000).normalized
  val threeSumExact = three.reduce(_ ++ _).normalized

  val chartSum = threeSum.showXY("Sample", title = "Sum of three d6", xLabel = "Sum")
  threeSumExact.plotXYOn(chartSum, "Exact")

  val threeMax = sampleMax(three, 1000).normalized
  val threeMaxExp = die(6).toCdf.pow(3).toPmf

  val chartMax = threeMax.showXY("Sample", title = "Max of three d6", xLabel = "Max")
  threeMaxExp.plotXYOn(chartMax, "Exponential")

  // mixture
  val five = Pmf(List(4, 6, 8, 12, 20).map(die))
  val mix = five.mixture

  mix.showBar("Outcome", title = "Outcome of random die from a box", xLabel = "Outcome")
}
