package thinkbayes.examples

import thinkbayes._

/** Application for solving the dice problem (page 19):
  *
  * "Suppose I have a box of dice that contains a 4-sided die, a 6-sided die, an 8-sided die, a 12-sided die, and a
  * 20-sided die. If you have ever played Dungeons & Dragons, you know what I am talking about.
  *
  * Suppose I select a die from the box at random, roll it, and get a 6. What is the probability that I rolled each
  * die?"
  */
object DiceApp extends App {

  case class Dice(hypos: Seq[Int]) extends SimpleSuite[Int, Int] {
    val pmf = Pmf(hypos)

    def likelihood(data: Int, hypo: Int) =
      if (hypo < data) 0 else 1.0 / hypo
  }

  // ---------

  val prior = new Dice(List(4, 6, 8, 12, 20))

  println("Priors:")
  prior.printChart()

  println()
  println("After a 6 is rolled:")
  val posterior = prior.observed(6)
  posterior.printChart()

  println()
  println("After 6, 8, 7, 7, 5, 4 are rolled after the first 6:")
  val posterior2 = posterior.observed(6, 8, 7, 7, 5, 4)
  posterior2.printChart()
}
