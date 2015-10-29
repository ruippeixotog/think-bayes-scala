package thinkbayes.extensions.distributions

import org.apache.commons.math3.special.Beta
import org.apache.commons.math3.special.Gamma
import org.apache.commons.math3.util.CombinatoricsUtils
import org.apache.commons.math3.util.FastMath._

object SpecialFunctions {

  @inline def gamma(x: Double) = Gamma.gamma(x)
  @inline def logGamma(x: Double) =
    if (x < 0.0) log(Gamma.gamma(x))
    else Gamma.logGamma(x)

  //  def logAbsGamma(x: Double) = {
  //    val sum = Gamma.lanczos(x)
  //    val tmp = x + Gamma.LANCZOS_G + .5
  //    ((x + .5) * log(tmp)) - tmp + 0.5 * log(2.0 * PI) + log(sum / x)
  //  }

  @inline def beta(p: Double, q: Double) = exp(Beta.logBeta(p, q))
  @inline def logBeta(p: Double, q: Double) = Beta.logBeta(p, q)

  @inline def fact(n: Int) = CombinatoricsUtils.factorial(n)
  @inline def logFact(n: Int) = CombinatoricsUtils.factorialLog(n)

  @inline def binomial(n: Int, k: Int) = CombinatoricsUtils.binomialCoefficient(n, k)
  @inline def logBinomial(n: Int, k: Int) = CombinatoricsUtils.binomialCoefficientLog(n, k)

  def realBinomial(x: Double, k: Int) =
    if (!x.isValidInt) gamma(x + 1) / (gamma(x - k + 1) * gamma(k + 1))
    else {
      if (x < 0.0) negBinomial(x.toInt, k)
      else if (k > x) 0.0
      else binomial(x.toInt, k).toDouble
    }

  def logRealBinomial(x: Double, k: Int) =
    if (!x.isValidInt) {
      val lg0 = logGamma(x + 1)
      val lg1 = logGamma(x - k + 1)
      val lg2 = logGamma(k + 1)
      //      val g0 = if (lg0.isNaN) log(abs(gamma(x + 1))) else lg0
      //      val g1 = if (lg1.isNaN) log(abs(gamma(x - k + 1))) else lg1
      //      val g2 = if (lg2.isNaN) log(abs(gamma(k + 1))) else lg2
      //
      //      if (g0.isNaN ^ g1.isNaN ^ g2.isNaN) Double.NaN else g0 - (g1 + g2)

      if (lg0.isNaN ^ lg1.isNaN ^ lg2.isNaN) Double.NaN else 1 - (log(x + 1) + logBeta(x - k + 1, k + 1))
    } else {
      if (x < 0.0) logNegBinomial(x.toInt, k)
      else if (k > x) log(0.0)
      else logBinomial(x.toInt, k)
    }

  private[this] def logNegBinomial(n: Int, k: Int) =
    log(pow(-1, k)) + logFact(-n + k - 1) - (logFact(-n - 1) + logFact(k))

  private[this] def negBinomial(n: Int, k: Int) =
    pow(-1, k) * exp(logFact(-n + k - 1) - (logFact(-n - 1) + logFact(k)))
}
