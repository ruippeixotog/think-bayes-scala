package thinkbayes.extensions

import org.jfree.chart.StandardChartTheme
import scalax.chart._
import scalax.chart.Charting._
import thinkbayes._
import scalax.chart.ChartFactories.BarChart

object Plotting {
  implicit val chartTheme = StandardChartTheme.createLegacyTheme()

  implicit class PlottablePmf[K](val pmf: Pmf[K]) extends AnyVal {

    def plotBar()(implicit ord: K => Ordered[K]): CategoryChart = {
      val chart = BarChart(pmf.hist.toSeq.toCategoryDataset)
      chart.rangeAxisLabel = "probability"
      chart.show
      chart
    }

    def plotXY(xLabel: String = "", seriesName: String = "")(implicit ord: Ordering[K], asNum: K => Number): XYChart = {
      val chart = XYLineChart(pmf.hist.toSeq.toXYSeriesCollection(seriesName))
      chart.domainAxisLabel = xLabel
      chart.rangeAxisLabel = "probability"
      chart.labelGenerator = None
      chart.show
      chart
    }
  }
}
