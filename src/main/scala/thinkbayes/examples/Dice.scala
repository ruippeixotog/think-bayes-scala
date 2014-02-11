package thinkbayes.examples

import thinkbayes.Suite

class Dice(hypos: Seq[Int]) extends Suite[Int, Int] {
  hypos.foreach(set(_, 1))
  normalize()

  def likelihood(data: Int, hypo: Int) =
    if(hypo < data) 0 else 1.0 / hypo
}

object DiceApp extends App {
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
