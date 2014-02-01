package thinkbayes.examples

import thinkbayes.Suite
import MM._

object MM {
  type Color = String
  type Mix = Map[Color, Int]
  type Bag = String

  val mix94 = Map(
    "brown" -> 30,
    "yellow" -> 20,
    "red" -> 20,
    "green" -> 10,
    "orange" -> 10,
    "tan" -> 10)

  val mix96 = Map(
    "blue" -> 24,
    "green" -> 20,
    "orange" -> 16,
    "yellow" -> 14,
    "red" -> 13,
    "brown" -> 13)
}

class MM(hypos: Seq[Char], hypoDefs: Map[Char, Map[Bag, Mix]]) extends Suite[Char, (Bag, Color)] {
  hypos.foreach(set(_, 1))
  normalize()

  def likelihood(data: (Bag, Color), hypo: Char) =
    hypoDefs(hypo)(data._1).getOrElse(data._2, 0).toDouble
}

object MMApp extends App {
  val hypoA = Map("bag1" -> mix94, "bag2" -> mix96)
  val hypoB = Map("bag1" -> mix96, "bag2" -> mix94)

  val suite = new MM("AB", Map('A' -> hypoA, 'B' -> hypoB))

  suite.update("bag1", "yellow")
  suite.update("bag2", "green")
  suite.print()
}
