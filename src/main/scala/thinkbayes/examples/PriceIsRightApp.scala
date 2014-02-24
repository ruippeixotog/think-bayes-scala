package thinkbayes.examples

import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import scala.io.Source
import thinkbayes._
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

  val maxPrice = 75000.0
  val step = 100.0

  case class Player(prices: Array[Int], bids: Array[Int], diffs: Array[Int]) {
    val showcasePdf = estimatePdf(prices)
    val showcasePmf = showcasePdf.toPmf(0.0 to maxPrice by step)
    val diffCdf = Cdf(diffs)

    val errorMean = 0.0
    val errorStdev = new StandardDeviation().evaluate(diffs.map(_.toDouble))
    val errorPdf = normalPdf(errorMean, errorStdev)

    def probOverbid: Double = diffCdf.prob(-1)
    def probWorseThan(diff: Int): Double = 1.0 - diffCdf.prob(diff)
  }

  case class Price(player: Player) extends Suite[Double, Double] {
    val pmf = player.showcasePmf

    def likelihood(guess: Double, showcase: Double): Double =
      player.errorPdf.density(showcase - guess)
  }

  case class GainCalculator(player: Player, opponent: Player, playerGuess: Int, opponentGuess: Int) {
    val playerSuite = Price(player).observed(playerGuess)
    val opponentSuite = Price(opponent).observed(opponentGuess)

    def probWin(diff: Int): Double = opponent.probOverbid + opponent.probWorseThan(diff)

    def gain(bid: Int, price: Int): Double =
      if(bid > price) 0
      else {
        val diff = price - bid
        val prob = probWin(diff)

        if(diff <= 250) 2 * price * prob
        else price * prob
      }

    def expectedGain(bid: Int): Double =
      playerSuite.hist.map { case (price, prob) => prob * gain(bid, price.toInt) }.sum

    def optimalBid: (Double, Double) =
      (0.0 to maxPrice by step).map { bid => (expectedGain(bid.toInt), bid) }.max.swap
  }

  // ---------

  def getDataFromCsv(file: String) = Source.fromFile(file).getLines().
    filter { line => line.matches(".*(Bid|Showcase|Difference).*") }.
    map(_.split(",").drop(1).map(_.toInt))

  val dataShowcase1 :: dataShowcase2 :: dataBids1 :: dataBids2 :: dataDiff1 :: dataDiff2 :: _ =
    getDataFromCsv("showcases.2011.csv").zip(getDataFromCsv("showcases.2012.csv")).
      map { case (row1, row2) => row1 ++ row2 }.toList

  val player1 = Player(dataShowcase1, dataBids1, dataDiff1)
  val player2 = Player(dataShowcase2, dataBids2, dataDiff2)

  // ---------

  println("Plotting the showcase price distributions...")
  val scChartTitle = "Prices of showcases 2011-2012"

  val scChart = player1.showcasePmf.plotXY("Showcase 1", title = scChartTitle, xLabel = "Price ($)")
  player2.showcasePmf.plotXYOn(scChart, "Showcase 2")

  // ---------

  println("Plotting the distribution of the bid errors...")
  val diffChartTitle = "Difference between the players' bid and the actual price"

  val diffChart = player1.diffCdf.plotXY("Player 1", title = diffChartTitle, xLabel = "Diff ($)")
  player2.diffCdf.plotXYOn(diffChart, "Player 2")

  // ---------

  println("Plotting prior and posterior distributions for player 1 based on a best guess of 20000$...")
  val guessChartTitle = "Distributions for player 1 based on a best guess of 20000$"

  val pricePrior = Price(player1)
  val guessChart = pricePrior.plotXY("Prior", title = guessChartTitle, xLabel = "Price ($)")

  val pricePosterior = pricePrior.observed(20000)
  pricePosterior.plotXYOn(guessChart, "Posterior")

  // ---------

  println()
  println("Optimal bids when the best guess of player 1 is 20000$ and the best guess of player 2 is 40000$:")

  val (optimal1, gain1) = GainCalculator(player1, player2, 20000, 40000).optimalBid
  val (optimal2, gain2) = GainCalculator(player2, player1, 40000, 20000).optimalBid

  println("Player 1 bid: %.2f$, with expected gain of %.2f$".format(optimal1, gain1))
  println("Player 2 bid: %.2f$, with expected gain of %.2f$".format(optimal2, gain2))
}
