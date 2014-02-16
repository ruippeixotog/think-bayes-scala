package thinkbayes.extensions

import org.jfree.chart.StandardChartTheme
import org.jfree.data.xy.XYSeriesCollection
import scalax.chart._
import scalax.chart.Charting._
import thinkbayes._

object Plotting {
  implicit val chartTheme = StandardChartTheme.createLegacyTheme()

  trait Plottable[K] {
    def values: Seq[(K, Double)]

    def plotBar()(implicit ord: K => Ordered[K]): CategoryChart = {
      val chart = BarChart(values.sorted.toCategoryDataset)
      chart.rangeAxisLabel = "probability"
      chart.show(dim = (800, 600))
      chart
    }

    def plotXY(seriesName: String, title: String = "", xLabel: String = "")(implicit ord: Ordering[K], asNum: K => Number): XYChart = {
      val chart = XYLineChart(values.toXYSeriesCollection(seriesName), title = title)
      chart.domainAxisLabel = xLabel
      chart.rangeAxisLabel = "probability"
      chart.show(dim = (800, 600))
      chart
    }

    def plotXYOn(chart: XYChart, seriesName: String)(implicit ord: Ordering[K], asNum: K => Number): chart.type = {
      chart.plot.getDataset match {
        case seriesList: XYSeriesCollection =>
          seriesList.addSeries(values.toXYSeries(seriesName))
      }
      chart
    }
  }

  implicit def pmfAsPlottable[K](pmf: Pmf[K]) = new Plottable[K] { def values = pmf.hist.toSeq }
  implicit def cdfAsPlottable[K](pmf: Cdf[K]) = new Plottable[K] { def values = pmf.vals }
}
