package thinkbayes.extensions

import org.specs2.mutable.Specification
import scala.util.Random
import thinkbayes._
import thinkbayes.extensions.Distributions._
import thinkbayes.extensions.Sampling._
import thinkbayes.extensions.Stats._

class SamplingSpec extends Specification {

  "A PmfSampling" should {

    "allow taking random samples from it" in {
      val nValues = 20
      val pmf = Pmf((0 until nValues).map(_ -> Random.nextDouble()).toMap).normalized
      val nSamples = 10000
      val samplePmf = Pmf(pmf.samplesIterator.take(nSamples))

      (0 until nValues).forall { value =>
        val p = pmf.prob(value)
        val (s, l) = binomialPmf(nSamples, p).credibleInterval(1.0 - 1.0 / 1e9)
        samplePmf.prob(value) must beBetween(s / nSamples.toDouble, l / nSamples.toDouble)
      }
    }
  }
}
