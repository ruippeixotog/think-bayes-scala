package thinkbayes.examples

import thinkbayes.Suite
import thinkbayes.extensions.Plotting._

class Locomotive(hypos: Seq[Int]) extends Suite[Int, Int] {
  hypos.foreach(set(_, 1))
  normalize()

  def likelihood(data: Int, hypo: Int) =
    if(hypo < data) 0 else 1.0 / hypo
}

/**
 * Application for solving the locomotive problem (page 20):
 *
 * "A railroad numbers its locomotives in order 1..N. One day you see a locomotive with
 * the number 60. Estimate how many locomotives the railroad has."
 */
object LocomotiveApp extends App {
  val suite = new Locomotive(1 to 1000)

  println("Priors (plot)...")
  suite.plotXY("Number of trains", "Prior")

  println("After a train with number 60 is seen (plot)...")
  suite.update(60)
  suite.plotXY("Number of trains", "After train #60")

  println("Mean of the distribution after #60 is seen: " + suite.mean)
}