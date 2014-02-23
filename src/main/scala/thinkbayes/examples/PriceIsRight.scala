package thinkbayes.examples

import scala.io.Source
import thinkbayes.extensions.Distributions._
import thinkbayes.extensions.Plotting._

/**
 * Application for studying the The Price is Right problem (page 51):
 *
 * "On November 1, 2007, contestants named Letia and Nathaniel appeared on The Price
 * is Right, an American game show. They competed in a game called The Showcase, where
 * the objective is to guess the price of a showcase of prizes. The contestant who comes
 * closest to the actual price of the showcase, without going over, wins the prizes.
 *
 * Nathaniel went first. His showcase included a dishwasher, a wine cabinet, a laptop computer,
 * and a car. He bid $26,000.
 *
 * Letia’s showcase included a pinball machine, a video arcade game, a pool table, and a
 * cruise of the Bahamas. She bid $21,500.
 *
 * The actual price of Nathaniel’s showcase was $25,347. His bid was too high, so he lost.
 *
 * The actual price of Letia’s showcase was $21,578. She was only off by $78, so she won
 * her showcase and, because her bid was off by less than $250, she also won Nathaniel’s
 * showcase.
 *
 * For a Bayesian thinker, this scenario suggests several questions:
 * <ol>
 *   <li>Before seeing the prizes, what prior beliefs should the contestant have about the
 *   price of the showcase?</li>
 *   <li>After seeing the prizes, how should the contestant update those beliefs?</li>
 *   <li>Based on the posterior distribution, what should the contestant bid?</li>
 * </ol>"
 *
 * Note: this application requires the files "showcases.2011.csv" and "showcases.2012.csv" to be
 * in the root folder of the project. The files are available at
 * [[http://thinkbayes.com/showcases.2011.csv]] and [[http://thinkbayes.com/showcases.2012.csv]].
 */
object PriceIsRightApp extends App {

  def getDataFromCsv(file: String) = Source.fromFile(file).getLines().
    filter { line => line.contains("Bid") || line.contains("Showcase") }.
    map(_.split(",").drop(1).map(_.toInt))

  val dataShowcase1 :: dataShowcase2 :: dataBids1 :: dataBids2 :: _ =
    getDataFromCsv("showcases.2011.csv").zip(getDataFromCsv("showcases.2012.csv")).
      map { case (row1, row2) => row1 ++ row2 }.toList

  val showcase1 = estimatePdf(dataShowcase1)
  val chart = showcase1.toPmf(0.0 to 80000.0 by 80.0).
    plotXY("Showcase 1", title = "Prices of showcases 2011-2012", xLabel = "Price ($)")

  val showcase2 = estimatePdf(dataShowcase2)
  showcase2.toPmf(0.0 to 80000.0 by 80.0).plotXYOn(chart, "Showcase 2")
}
