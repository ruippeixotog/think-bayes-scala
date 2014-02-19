package thinkbayes.examples

import thinkbayes.Pmf
import thinkbayes.extensions.Plotting._
import thinkbayes.extensions.Sampling._

class Die(sides: Int) extends Pmf[Int] {
  (1 to sides).foreach(set(_, 1))
  normalize()
}

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
  val three = Seq.fill(3)(new Die(6))

  val threeSum = sampleSum(three, 1000)
  threeSum.normalize()

  val threeSumExact = three.reduce[Pmf[Int]](_ + _)
  threeSumExact.normalize()

  val chartSum = threeSum.plotXY("Sample", title = "Sum of three d6", xLabel = "Sum")
  threeSumExact.plotXYOn(chartSum, "Exact")

  val threeMax = sampleMax(three, 1000)
  threeMax.normalize()

  val threeMaxExp = new Die(6).toCdf.pow(3).toPmf

  val chartMax = threeMax.plotXY("Sample", title = "Max of three d6", xLabel = "Max")
  threeMaxExp.plotXYOn(chartMax, "Exponential")
}
