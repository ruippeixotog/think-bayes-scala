package thinkbayes.examples

import thinkbayes.Suite

/**
 * Application for solving the dice problem (page 19):
 *
 * "Suppose I have a box of dice that contains a 4-sided die, a 6-sided die, an 8-sided die, a
 * 12-sided die, and a 20-sided die. If you have ever played Dungeons & Dragons, you know
 * what I am talking about.
 *
 * Suppose I select a die from the box at random, roll it, and get a 6. What is the probability
 * that I rolled each die?"
 */
object DiceApp extends App {

  class Dice(hypos: Seq[Int]) extends Suite[Int, Int] {
    hypos.foreach(set(_, 1))
    normalize()

    def likelihood(data: Int, hypo: Int) =
      if(hypo < data) 0 else 1.0 / hypo
  }

  // ---------

  val suite = new Dice(List(4, 6, 8, 12, 20))

  println("Priors:")
  suite.printChart()

  println()
  println("After a 6 is rolled:")
  suite.update(6)
  suite.printChart()

  println()
  println("After 6, 8, 7, 7, 5, 4 are rolled after the first 6:")
  List(6, 8, 7, 7, 5, 4).foreach(suite.update)
  suite.printChart()
}
