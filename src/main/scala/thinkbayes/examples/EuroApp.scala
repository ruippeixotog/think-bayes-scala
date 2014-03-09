package thinkbayes.examples

import thinkbayes._
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

  def trianglePmf(unit: Double = 1.0): Pmf[Double] = {
    val hist = (0.0 to 100.0 by unit).map { hypo =>
      (hypo, if(hypo <= 50.0) hypo else 100.0 - hypo)
    }.toMap

    Pmf(hist).normalized
  }

  case class Euro(unit: Double = 1.0, triangle: Boolean = false) extends Suite[Double, CoinSide] {
    val pmf = if(triangle) trianglePmf(unit) else Pmf(0.0 to 100.0 by unit)

    override def likelihood(data: CoinSide, hypo: Double) =
      (if(data == Heads) hypo else 100.0 - hypo) / 100.0
  }

  // ---------

  val unifPrior = Euro()
  val triPrior = Euro(triangle = true)

  println("Plotting priors...")
  val priorPlot = unifPrior.plotXY("Uniform", title = "Prior", xLabel = "Probability of heads (%)")
  triPrior.plotXYOn(priorPlot, "Triangle")

  println("Plotting posteriors after 140 heads and 110 tails are seen...")
  val dataset = Seq.fill(140)(Heads) ++ Seq.fill(110)(Tails)
  val unifPosterior = unifPrior.observedSet(dataset)
  val triPosterior = triPrior.observedSet(dataset)

  val postPlot = unifPosterior.plotXY("Uniform", title = "Posterior", xLabel = "Probability of heads (%)")
  triPosterior.plotXYOn(postPlot, "Triangle")

  println("Posterior distribution stats with uniform prior:")
  println("Hypothesis with highest probability: " + unifPosterior.pmf.maxProb._1)
  println("Mean of the distribution: " + unifPosterior.pmf.mean)
  println("Median of the distribution: " + unifPosterior.pmf.percentile(0.5))
  println("90%% credible interval: " + unifPosterior.pmf.credibleInterval(0.9))

  println("Posterior distribution stats with triangle prior:")
  println("Hypothesis with highest probability: " + triPosterior.pmf.maxProb._1)
  println("Mean of the distribution: " + triPosterior.pmf.mean)
  println("Median of the distribution: " + triPosterior.pmf.percentile(0.5))
  println("90%% credible interval: " + triPosterior.pmf.credibleInterval(0.9))

  println("Plotting posterior using a beta distribution...")
  val beta = new Beta()
  beta.updateSet(140, 110)
  beta.toPmf().plotXY("Beta", title = "Beta distribution", xLabel = "Probability of heads")
}
