package thinkbayes.examples

import thinkbayes.Suite

/**
 * Application for solving the Monty Hall problem (page 7):
 *
 * "Monty Hall was the original host of the game show Let’s Make a Deal. The Monty Hall
 * problem is based on one of the regular games on the show. If you are on the show, here’s
 * what happens:
 *
 * <ul>
 *   <li>Monty shows you three closed doors and tells you that there is a prize behind each
 *   door: one prize is a car, the other two are less valuable prizes like peanut butter and
 *   fake finger nails. The prizes are arranged at random.</li>
 *   <li>The object of the game is to guess which door has the car. If you guess right, you
 *   get to keep the car.</li>
 *   <li>You pick a door, which we will call Door A. We’ll call the other doors B and C.
 *   Before opening the door you chose, Monty increases the suspense by opening either
 *   Door B or C, whichever does not have the car. (If the car is actually behind Door
 *   A, Monty can safely open B or C, so he chooses one at random.)</li>
 *   <li>Then Monty offers you the option to stick with your original choice or switch to
 *   the one remaining unopened door.</li>
 * </ul>
 *
 * The question is, should you “stick” or “switch” or does it make no difference?"
 */
object MontyApp extends App {

  class Monty(hypos: Seq[Char], firstChoice: Char) extends Suite[Char, Char] {
    hypos.foreach(set(_, 1))
    normalize()

    def likelihood(opened: Char, hypo: Char) =
      if (opened == hypo) 0 // if the door was opened, it is surely not the winning door
      else if (hypo == firstChoice) 1.0 / (hypos.length - 1) // Monty can open any door other than the winning one
      else 1.0 / (hypos.length - 2) // Monty can open any door other than the winning one and the chosen one
  }

  // ---------

  val suite = new Monty("ABC", 'A') // doors A, B and C, first choice is A

  println("Before any door is opened:")
  suite.printChart() // print the probability of each hypothesis

  println()
  println("After Monty opens door B:")
  suite.update('B') // Monty opens B
  suite.printChart() // print the probability of each hypothesis
}
