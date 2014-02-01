package thinkbayes.examples

import thinkbayes.Suite

class Monty(hypos: Seq[Char], firstChoice: Char) extends Suite[Char, Char] {
  hypos.foreach(set(_, 1))
  normalize()

  def likelihood(opened: Char, hypo: Char) =
    if (opened == hypo) 0 // if the door was opened, it is surely not the winning door
    else if (hypo == firstChoice) 1.0 / (hypos.length - 1) // Monty can open any door other than the winning one
    else 1.0 / (hypos.length - 2) // Monty can open any door other than the winning one and the chosen one
}

object MontyApp extends App {
  val suite = new Monty("ABC", 'A') // doors A, B and C, first choice is A

  println("Before any door is opened:")
  suite.print() // print the probability of each hypothesis

  println()
  println("After Monty opens door B:")
  suite.update('B') // Monty opens B
  suite.print() // print the probability of each hypothesis
}
