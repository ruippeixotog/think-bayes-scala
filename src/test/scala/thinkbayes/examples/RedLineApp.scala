package thinkbayes.examples

import thinkbayes._
import thinkbayes.extensions.Distributions._
import thinkbayes.extensions.Plotting._

/** Application for studying the The Red Line problem (page 77):
  *
  * "In Massachusetts, the Red Line is a subway that connects Cambridge and Boston. When I was working in Cambridge I
  * took the Red Line from Kendall Square to South Station and caught the commuter rail to Needham. During rush hour Red
  * Line trains run every 7–8 minutes, on average.
  *
  * When I arrived at the station, I could estimate the time until the next train based on the number of passengers on
  * the platform. If there were only a few people, I inferred that I just missed a train and expected to wait about 7
  * minutes. If there were more passengers, I expected the train to arrive sooner. But if there were a large number of
  * passengers, I suspected that trains were not running on schedule, so I would go back to the street level and get a
  * taxi.
  *
  * While I was waiting for trains, I thought about how Bayesian estimation could help predict my wait time and decide
  * when I should give up and take a taxi. This chapter presents the analysis I came up with."
  */
object RedLineApp extends App {

  val observedGapTimes = List(428.0, 705.0, 407.0, 465.0, 433.0, 425.0, 204.0, 506.0, 143.0, 351.0, 450.0, 598.0, 464.0,
    749.0, 341.0, 586.0, 754.0, 256.0, 378.0, 435.0, 176.0, 405.0, 360.0, 519.0, 648.0, 374.0, 483.0, 537.0, 578.0,
    534.0, 577.0, 619.0, 538.0, 331.0, 186.0, 629.0, 193.0, 360.0, 660.0, 484.0, 512.0, 315.0, 457.0, 404.0, 740.0,
    388.0, 357.0, 485.0, 567.0, 160.0, 428.0, 387.0, 901.0, 187.0, 622.0, 616.0, 585.0, 474.0, 442.0, 499.0, 437.0,
    620.0, 351.0, 286.0, 373.0, 232.0, 393.0, 745.0, 636.0, 758.0).map(_ / 60.0)

  val prec = 10.0 / 60.0

  def biasPmf(zPmf: Pmf[Double]): Pmf[Double] =
    zPmf.map { case (k, prob) => (k, prob * k) }.normalized

  def waitTimePmf(zbPmf: Pmf[Double]): Pmf[Double] =
    zbPmf.mapKeys { k => Pmf(0.0 to k by prec) }.mixture

  case class WaitTimeCalculator(zPmf: Pmf[Double]) {
    val zbPmf = biasPmf(zPmf)
    val xPmf = waitTimePmf(zbPmf)
    val yPmf = xPmf
  }

  case class ElapsedTime(pmf: Pmf[Double]) extends SimpleSuite[Double, (Double, Int)] {
    def likelihood(data: (Double, Int), x: Double) = poissonPmf(data._1 * x).prob(data._2)
  }

  case class ElapsedTimeEstimator(calc: WaitTimeCalculator, lam: Double = 2.0, numPasengers: Int = 15) {

    def predictWaitTime(xPmf: Pmf[Double]): Pmf[Double] =
      (calc.zbPmf -- xPmf).filterKeys(_ >= 0.0).normalized

    val xPriorSuite = ElapsedTime(calc.xPmf)
    val xPostSuite = xPriorSuite.observed((lam, numPasengers))
    val yPmf = predictWaitTime(xPostSuite.pmf)
  }

  val observedArrivalRates = List((17, 4.6, 9), (22, 1.0, 0), (23, 1.4, 4), (18, 5.4, 12), (4, 5.8, 11))

  case class ArrivalRate(hypos: Seq[Double]) extends SimpleSuite[Double, (Double, Int)] {
    val pmf = Pmf(hypos)
    def likelihood(data: (Double, Int), lam: Double) = poissonPmf(lam * data._1).prob(data._2)
  }

  case class ArrivalRateEstimator(data: Seq[(Int, Double, Int)]) {

    val lamPriorSuite = ArrivalRate(0.0 to 5.0 by prec)
    val lamPostSuite = lamPriorSuite.observedSet(data.map { case (k1, y, k2) => (y, k2) })
  }

  // ---------

  println("Plotting the distribution of gap time between trains...")
  val gapChartTitle = "Time between trains"

  val zPmf = estimatePdf(observedGapTimes).toPmf(0.0 to 20.0 by prec / 2)
  val calc = WaitTimeCalculator(zPmf)

  val gapChart = zPmf.showXY("Actual (z)", title = gapChartTitle, xLabel = "Minutes")
  calc.zbPmf.plotXYOn(gapChart, "As seen by passengers (zb)")

  println("Plotting the CDF of gap and wait times...")
  val cdfTimesChartTitle = "CDF of gap and wait times"

  val cdfTimesChart = zPmf.toCdf.showXY("Actual gap time (z)", title = cdfTimesChartTitle, xLabel = "Minutes")

  calc.zbPmf.toCdf.plotXYOn(cdfTimesChart, "Biased gap time (zb)")
  calc.xPmf.toCdf.plotXYOn(cdfTimesChart, "Wait time (y)")

  println(
    "Plotting the CDF of wait times after seeing 15 passengers and considering 2 arrivals " +
      "per minute..."
  )
  val postWaitChartTitle = "CDF of wait times after seeing 15 passengers, 2 arrivals/min"

  val ete = ElapsedTimeEstimator(calc)

  val postWaitChart =
    ete.xPriorSuite.pmf.toCdf.showXY("Prior x", title = postWaitChartTitle, xLabel = "Wait time (min)")

  ete.xPostSuite.pmf.toCdf.plotXYOn(postWaitChart, "Posterior x")
  ete.yPmf.toCdf.plotXYOn(postWaitChart, "Predicted y")

  // ---------

  println("Plotting the distribution of the arrival rate after five days of passenger data...")
  val arrivalRatesChartTitle = "CDF of arrival rates after five days of passenger data"

  val are = ArrivalRateEstimator(observedArrivalRates)

  val arrivalRatesChart = are.lamPriorSuite.pmf.toCdf
    .showXY("Prior λ", title = arrivalRatesChartTitle, xLabel = "Arrival rate (passengers / min)")

  are.lamPostSuite.pmf.toCdf.plotXYOn(arrivalRatesChart, "Posterior λ")

  // ---------

  println("Plotting the predictive distribution of y....")
  val predWaitChartTitle = "CDF of wait times considering the distribution of λ"

  val yPredPmf = are.lamPostSuite.pmf.mapKeys(ElapsedTimeEstimator(calc, _).yPmf).mixture

  val predWaitChart = yPredPmf.toCdf.showXY("Mix", title = predWaitChartTitle, xLabel = "Wait time (min)")
}
