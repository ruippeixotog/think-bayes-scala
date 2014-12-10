# Think Bayes in Scala

A Scala implementation of the classes and functions used in the great book _Think Bayes_ by Allen B. Downey, available for free (and open-source) [here](http://www.greenteapress.com/thinkbayes/).

## Quick start

The code in this repository is available as a library and can be used in Scala 2.11.x projects by adding the following dependency to `build.sbt`:

```
libraryDependencies += "net.ruippeixotog" %% "think-bayes" % "0.1"
```

## Core classes

### Probability mass functions

The `Pmf` class is arguably the core collection in _Think Bayes_, due to the latter's focus on problem solving using discrete approximations instead of continuous mathematics. The way to build a `Pmf` and manipulate it is pretty simple:

```scala
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

```scala
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

```scala
  scala> def die(n: Int) = Pmf(1 to n)
  die: (n: Int)thinkbayes.Pmf[Int]

  scala> die(6)
  res8: thinkbayes.Pmf[Int] = Map(5 -> 0.16666666666666666, 1 -> 0.16666666666666666, 6 -> 0.16666666666666666, 2 -> 0.16666666666666666, 3 -> 0.16666666666666666, 4 -> 0.16666666666666666)

  scala> die(6).mean
  res9: Double = 3.5

  scala> (die(6) ++ die(6)).printChart() // sum of two dice
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

  scala> val bag = Pmf(List(die(4), die(6), die(8), die(12), die(20))) // a bag containing 5 different dice
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

The `Distributions` extension provides methods for creating common `Pmf` such as Gaussian or Poisson distributions.

### Bayesian suites

The implementation of `Suite` provided in this library does not extend `Pmf`; it is rather provided as a trait which applications can implement to model specific problems:

```scala
  scala> case class Dice(hypos: Seq[Int]) extends Suite[Int, Int] { // which dice from `hypos` are we rolling?
       |   val pmf = Pmf(hypos)
       |   def likelihood(data: Int, hypo: Int) = if(hypo < data) 0 else 1.0 / hypo
       | }
  defined class Dice

  scala> val prior = Dice(List(4, 6, 8, 12, 20))
  prior: Dice = Dice(List(4, 6, 8, 12, 20))

  scala> prior.printChart()
  4  0.2    ##########
  6  0.2    ##########
  8  0.2    ##########
  12 0.2    ##########
  20 0.2    ##########

  scala> val posterior = prior.observed(6) // after a 6 is rolled
  posterior: thinkbayes.Suite[Int,Int] = thinkbayes.Suite$$anon$1@120fb03e

  scala> posterior.printChart()
  4  0.0
  6  0.3921 ###################
  8  0.2941 ##############
  12 0.1960 #########
  20 0.1176 #####
```

The same prior could be built directly with:

```scala
  scala> val prior = Suite[Int, Int](Pmf(List(4, 6, 8, 12, 20))) { (d, h) =>
       |   if (h < d) 0 else 1.0 / h
       | }
  prior: thinkbayes.Suite[Int,Int]{val pmf: thinkbayes.Pmf[Int]} = thinkbayes.Suite$$anon$1@130dd39f
```

Multiple observations can be given to the `Suite` in bulk, which can yield results more stable numerically:

```scala
  scala> posterior.observed(6, 8, 7, 7, 5, 4).printChart()
  4  0.0
  6  0.0
  8  0.9432 ###############################################
  12 0.0552 ##
  20 0.0015
```

### Cumulative distribution functions

A `Cdf` can be created just like a `Pmf`. It supports efficient querying for the cumulative probability on a given value (`prob`) and for the value at a given percentile (`value`):

```scala
  scala> val cdf = Cdf('a' -> 0.2, 'b' -> 0.2, 'c' -> 0.6)
  cdf: thinkbayes.Cdf[Char] = Cdf(Vector((a,0.2), (b,0.4), (c,1.0)))

  scala> cdf.prob('b')
  res10: Double = 0.4

  scala> cdf.value(0.5)
  res11: Char = c

  scala> cdf.value(0.35)
  res12: Char = b

  scala> cdf.printChart()
  a 0.2    ##########
  b 0.4    ####################
  c 1.0    ##################################################
```

Unlike `Pmf`, `Cdf` does not implement the `Map` trait and, therefore, does not inherit the common Scala collection methods. If you need to use those, you can convert easily a `Cdf` to and from a `Pmf`:

```scala
  scala> cdf.toPmf
  res13: thinkbayes.Pmf[Char] = Map(a -> 0.2, b -> 0.2, c -> 0.6)

  scala> cdf.toPmf.toCdf
  res14: thinkbayes.Cdf[Char] = Cdf(Vector((a,0.2), (b,0.4), (c,1.0)))
```

### Probability density functions

A `Pdf` can be created from a Scala real-valued function and provides a `density` method for calculating the density at a given value:

```scala
  scala> val pdf = Pdf { x => math.max(-x * x + 1, 0) }
  pdf: thinkbayes.Pdf = thinkbayes.Pdf$$anon$3@744cb6e3

  scala> pdf.density(0)
  res15: Double = 1.0

  scala> pdf.density(0.5)
  res16: Double = 0.75
```

A `BoundedPdf` is a `Pdf` whose domain has known lower and upper bounds.

```scala
  scala> val bpdf = Pdf(-1.0, 1.0) { x => math.max(-x * x + 1, 0) }
  bpdf: thinkbayes.BoundedPdf{val lowerBound: Double; val upperBound: Double} = thinkbayes.Pdf$$anon$2@397820d5
```

Both can be converted to a `Pmf` given a range or sequence of discrete values to compute. A `BoundedPdf` can alternatively be given a step value only. In both cases, the probabilities of the returned `Pmf` are normalized:

```scala
  scala> pdf.toPmf(0.0 to 1.0 by 0.1).printChart()
  0.0                 0.1398 ######
  0.1                 0.1384 ######
  0.2                 0.1342 ######
  0.30000000000000004 0.1272 ######
  0.4                 0.1174 #####
  0.5                 0.1048 #####
  0.6000000000000001  0.0895 ####
  0.7000000000000001  0.0713 ###
  0.8                 0.0503 ##
  0.9                 0.0265 #
  1.0                 0.0

  scala> bpdf.toPmf(0.2).printChart()
  -1.0                 0.0
  -0.8                 0.0545 ##
  -0.6                 0.0969 ####
  -0.3999999999999999  0.1272 ######
  -0.19999999999999996 0.1454 #######
  0.0                  0.1515 #######
  0.20000000000000018  0.1454 #######
  0.40000000000000013  0.1272 ######
  0.6000000000000001   0.0969 ####
  0.8                  0.0545 ##
  1.0                  0.0
```

The `Distributions` extension provides methods for creating common `Pdf` such as Gaussian or Exponential distributions.

## Extensions

This library was designed such that only the core operations needed for the creation and manipulation of the structures presented above are included in the class themselves. Additional features can be added by importing modules from the package `extensions`.

### Plotting

The `Plotting` module provides support for graphical plotting, leveraging the powerful [JFreeChart](http://www.jfree.org/jfreechart/) library with a custom theme. `Pmf`, `Suite`, `Cdf` and `BoundedPdf` instances can be plotted, as long as their keys have an `Ordering` (for plotting bar charts) or `Numeric` (for plotting XY line charts) implicit in scope:

```scala
  scala> import thinkbayes.extensions.Plotting._
  import thinkbayes.extensions.Plotting._

  scala> val xyChart = bpdf.plotXY("-x^2 + 1")
  xyChart: scalax.chart.XYChart = scalax.chart.ChartFactories$XYLineChart$$anon$17@290e640d
```

![plotxy](http://i.imgur.com/rG1d1vj.png)

```scala
  scala> val barChart = prior.plotBar("prior")
  barChart: scalax.chart.CategoryChart = scalax.chart.ChartFactories$BarChart$$anon$3@5c3e1ebe
```

![plotbar_prior](http://i.imgur.com/etUUT9a.png)

New series can be added to a previously created chart. This is useful for comparing differences between two distributions or Bayesian suites:

```scala
  scala> posterior.plotBarOn(barChart, "after a 6 is rolled")
  res17: barChart.type = scalax.chart.ChartFactories$BarChart$$anon$3@5c3e1ebe
```

![plotbar_posterior](http://i.imgur.com/7Ak0pQu.png)

Other attributes of the chart, such as the title and the axis labels, can be optionally specified.

### Distributions

The `Distributions` module provides integration with the distribution implementations from [Apache Commons Math](http://commons.apache.org/proper/commons-math/), as well as several methods for creating `Pmf` and `Pdf` instances for common distributions:

```scala
  scala> import thinkbayes.extensions.Distributions._
  import thinkbayes.extensions.Distributions._

  scala> poissonPmf(3.0).plotBar("")
  res18: scalax.chart.CategoryChart = scalax.chart.ChartFactories$BarChart$$anon$3@6736cd9d
```

![poisson](http://i.imgur.com/IkDOt6Z.png)

```scala
  scala> val tri: Pdf = new org.apache.commons.math3.distribution.TriangularDistribution(0.0, 0.5, 2.0)
  tri: thinkbayes.Pdf = thinkbayes.extensions.Distributions$$anon$1@7b5cdeb6

  scala> tri.bounded(0.0, 2.0).plotXY("")
  res19: scalax.chart.XYChart = scalax.chart.ChartFactories$XYLineChart$$anon$17@55a7c8a3
```
![triangular](http://i.imgur.com/8WW3cjU.png)

Finally, we can estimate a `Pdf` from a sequence of samples using kernel density estimation:

```scala
  scala> estimatePdf(Seq(1, 2, 2, 4, 4, 4, 9, 9, 9, 9, 11, 11, 15, 19)).bounded(0, 20).plotXY("")
  res20: scalax.chart.XYChart = scalax.chart.ChartFactories$XYLineChart$$anon$17@1c15725
```

![kde](http://i.imgur.com/ijBYGPg.png)

### Stats

The `Stats` module is a simple extension that provides the calculation of percentiles and credible intervals to `Pmf` and `Cdf` instances:

```scala
  scala> import thinkbayes.extensions.Stats._
  import thinkbayes.extensions.Stats._

  scala> normalPmf(2.5, 1.5).percentile(0.5)
  res21: Double = 2.5

  scala> normalPmf(0.0, 1.0).credibleInterval(0.9)
  res22: (Double, Double) = (-1.6440000000000001,1.6440000000000001)
```

### Sampling

Using `Pmf` merging methods such as `mixture` or `join` yield results as accurate as they can be, but they are also computationally expensive. The `Sampling` module aims to provide probabilistic alternatives based on sampling, which can be the only choice for large `Pmf`:

```scala
  scala> val dieList = Seq.fill(100)(die(6)) // a hundred dice
  dieList: Seq[thinkbayes.Pmf[Int]] = List(Map(5 -> 0.16666666666666666, 1 -> 0.16666666666666666, 6 -> 0.16666666666666666, 2 -> 0.16666666666666666, 3 -> 0.1666666666666666, 4 -> 0.16666666666666666),...

  scala> val xyChart = dieList.reduce(_ ++ _).plotXY("exact")
  xyChart: scalax.chart.XYChart = scalax.chart.ChartFactories$XYLineChart$$anon$17@30015846

  scala> sampleSum(dieList, 10000).plotXYOn(xyChart, "sampled")
  res23: xyChart.type = scalax.chart.ChartFactories$XYLineChart$$anon$17@81f0a53
```

![sampling](http://i.imgur.com/LiYUyFL.png)

## Examples

A number of examples and problems explored throughout _Think Bayes_ are implemented in the package `examples` in the [test directory](https://github.com/ruippeixotog/think-bayes-scala/tree/master/src/test/scala/thinkbayes/examples). They are always accompanied by the original problem description and I made an effort to make the steps of each problem as clear as possible.
