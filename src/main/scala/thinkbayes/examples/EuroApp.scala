package thinkbayes.examples

import thinkbayes.Suite
import thinkbayes.extensions.Plotting._
import thinkbayes.extensions.Stats._
import thinkbayes.utils.Beta

/**
 * Application for solving the Euro problem (page 29):
 *
 * "When spun on edge 250 times, a Belgian one-euro coin came up heads 140
 * times and tails 110. ‘It looks very suspicious to me,’ said Barry Blight, a statistics
 * lecturer at the London School of Economics. ‘If the coin were unbiased, the
 * chance of getting a result as extreme as that would be less than 7%.’
 *
 * But do these data give evidence that the coin is biased rather than fair?"
 */
object EuroApp extends App {

  type CoinSide = Boolean
  val Heads = true
  val Tails = false

  class Euro(unit: Double = 1.0) extends Suite[Double, CoinSide] {
    (0.0 to 100.0 by unit).foreach(incr(_))
    normalize()

    def setTrianglePrior() {
      (0.0 to 100.0 by unit).foreach { hypo =>
        set(hypo, if(hypo <= 50.0) hypo else 100.0 - hypo)
      }
      normalize()
    }

    override def likelihood(data: CoinSide, hypo: Double) =
      (if(data == Heads) hypo else 100.0 - hypo) / 100.0
  }

  // ---------

  val suite = new Euro()
  val suite2 = new Euro()
  suite2.setTrianglePrior()

  println("Plotting priors...")
  val priorPlot = suite.plotXY("Uniform", title = "Prior", xLabel = "Probability of heads (%)")
  suite2.plotXYOn(priorPlot, "Triangle")

  println("Plotting posteriors after 140 heads and 110 tails are seen...")
  val dataset = Seq.fill(140)(Heads) ++ Seq.fill(110)(Tails)
  suite.updateSet(dataset)
  suite2.updateSet(dataset)

  val postPlot = suite.plotXY("Uniform", title = "Posterior", xLabel = "Probability of heads (%)")
  suite2.plotXYOn(postPlot, "Triangle")

  println("Posterior distribution stats with uniform prior:")
  println("Hypothesis with highest probability: " + suite.max)
  println("Mean of the distribution: " + suite.mean)
  println("Median of the distribution: " + suite.percentile(0.5))
  println("90%% credible interval: " + suite.credibleInterval(0.9))

  println("Posterior distribution stats with triangle prior:")
  println("Hypothesis with highest probability: " + suite2.max)
  println("Mean of the distribution: " + suite2.mean)
  println("Median of the distribution: " + suite2.percentile(0.5))
  println("90%% credible interval: " + suite2.credibleInterval(0.9))

  println("Plotting posterior using a beta distribution...")
  val beta = new Beta()
  beta.updateSet(140, 110)
  beta.toPmf().plotXY("Beta", title = "Beta distribution", xLabel = "Probability of heads")
}
