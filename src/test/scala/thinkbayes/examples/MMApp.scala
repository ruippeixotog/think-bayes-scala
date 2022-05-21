package thinkbayes.examples

import thinkbayes._

/** Application for solving the M&M problem (page 6):
  *
  * "M&M’s are small candy-coated chocolates that come in a variety of colors. Mars, Inc., which makes M&M’s, changes
  * the mixture of colors from time to time.
  *
  * In 1995, they introduced blue M&M’s. Before then, the color mix in a bag of plain M&M’s was 30% Brown, 20% Yellow,
  * 20% Red, 10% Green, 10% Orange, 10% Tan. Afterward it was 24% Blue , 20% Green, 16% Orange, 14% Yellow, 13% Red, 13%
  * Brown.
  *
  * Suppose a friend of mine has two bags of M&M’s, and he tells me that one is from 1994 and one from 1996. He won’t
  * tell me which is which, but he gives me one M&M from each bag. One is yellow and one is green. What is the
  * probability that the yellow one came from the 1994 bag?"
  */
object MMApp extends App {

  type Color = String
  type Mix = Map[Color, Int]
  type Bag = String

  val mix94 = Map("brown" -> 30, "yellow" -> 20, "red" -> 20, "green" -> 10, "orange" -> 10, "tan" -> 10)

  val mix96 = Map("blue" -> 24, "green" -> 20, "orange" -> 16, "yellow" -> 14, "red" -> 13, "brown" -> 13)

  case class MM(hypos: Seq[Char], hypoDefs: Map[Char, Map[Bag, Mix]]) extends SimpleSuite[Char, (Bag, Color)] {
    val pmf = Pmf(hypos)

    def likelihood(data: (Bag, Color), hypo: Char) =
      hypoDefs(hypo)(data._1).getOrElse(data._2, 0).toDouble
  }

  // ---------

  val hypoA = Map("bag1" -> mix94, "bag2" -> mix96)
  val hypoB = Map("bag1" -> mix96, "bag2" -> mix94)

  val prior = MM("AB", Map('A' -> hypoA, 'B' -> hypoB))

  val posterior = prior.observed("bag1" -> "yellow", "bag2" -> "green")
  posterior.printChart()
}
