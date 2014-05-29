# Think Bayes in Scala

A Scala implementation of the classes and functions used in the great book "Think Bayes" by Allen B. Downey, available for free (and open-source) [here](http://www.greenteapress.com/thinkbayes/).

## Core classes

### Probability mass functions

The `Pmf` class is arguably the core collection in Think Bayes, due to the latter's focus on problem solving using discrete approximations instead of continuous mathematics. The way to build a `Pmf` and manipulate it is pretty simple:

```
  scala> import thinkbayes._
  import thinkbayes._

  scala> val pmf = Pmf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
  pmf: thinkbayes.Pmf[Char] = Map(a -> 0.2, b -> 0.2, c -> 0.6)

  scala> pmf.prob('a')
  res0: Double = 0.2

  scala> pmf.prob(_ < 'c')
  res1: Double = 0.4

  scala> pmf.random()
  res2: Char = c

  scala> pmf.printChart()
  a 0.2    ##########
  b 0.2    ##########
  c 0.6    ##############################
```

A `Pmf` is implemented as an immutable map and can be used as such:

```
  scala> pmf.size
  res3: Int = 3

  scala> pmf.map { case (k, v) => ((k + 1).toChar, v) }
  res4: thinkbayes.Pmf[Char] = Map(b -> 0.2, c -> 0.2, d -> 0.6)

  scala> pmf.filter(_._1 == 'a').normalized
  res5: thinkbayes.Pmf[Char] = Map(a -> 1.0)

  scala> pmf.foldLeft("")(_ + _._1)
  res6: String = abc

  scala> pmf.toList
  res7: List[(Char, Double)] = List((a,0.2), (b,0.2), (c,0.6))
```

Specialized `Pmf` merging methods can model more complex problems in a very concise manner:

```
  scala> def die(n: Int) = Pmf(1 to n)
  die: (n: Int)thinkbayes.Pmf[Int]

  scala> die(6)
  res8: thinkbayes.Pmf[Int] = Map(5 -> 0.16666666666666666, 1 -> 0.16666666666666666, 6 -> 0.16666666666666666, 2 -> 0.16666666666666666, 3 -> 0.16666666666666666, 4 -> 0.16666666666666666)

  scala> die(6).mean
  res9: Double = 3.5

  scala> (die(6) ++ die(6)).printChart() // sum of two dices
  2  0.0277 #
  3  0.0555 ##
  4  0.0833 ####
  5  0.1111 #####
  6  0.1388 ######
  7  0.1666 ########
  8  0.1388 ######
  9  0.1111 #####
  10 0.0833 ####
  11 0.0555 ##
  12 0.0277 #

  scala> val bag = Pmf(List(die(4), die(6), die(8), die(12), die(20))) // a bag containing 5 different dices
  bag: thinkbayes.Pmf[thinkbayes.Pmf[Int]] = Map(Map(5 -> 0.08333333333333333, 10 -> 0.08333333333333333, 1 -> 0.08333333333333333, 6 -> 0.08333333333333333, 9 -> ...

  scala> bag.mixture.printChart() // roll of a random die from the bag
  1  0.135  ######
  2  0.135  ######
  3  0.135  ######
  4  0.135  ######
  5  0.0850 ####
  6  0.0850 ####
  7  0.0516 ##
  8  0.0516 ##
  9  0.0266 #
  10 0.0266 #
  11 0.0266 #
  12 0.0266 #
  13 0.0100
  14 0.0100
  15 0.0100
  16 0.0100
  17 0.0100
  18 0.0100
  19 0.0100
  20 0.0100
```
