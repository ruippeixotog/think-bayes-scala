package thinkbayes.extensions

import org.jfree.chart.StandardChartTheme
import org.jfree.data.xy.XYSeriesCollection
import scalax.chart._
import scalax.chart.Charting._
import thinkbayes._

object Plotting {
  implicit val chartTheme = StandardChartTheme.createLegacyTheme()

  implicit class PlottablePmf[K](val pmf: Pmf[K]) extends AnyVal {

    def plotBar()(implicit ord: K => Ordered[K]): CategoryChart = {
      val chart = BarChart(pmf.hist.toSeq.sorted.toCategoryDataset)
      chart.rangeAxisLabel = "probability"
      chart.show(dim = (800, 600))
      chart
    }

    def plotXY(seriesName: String = "", title: String = "", xLabel: String = "")(implicit ord: Ordering[K], asNum: K => Number): XYChart = {
      val chart = XYLineChart(pmf.hist.toSeq.toXYSeriesCollection(seriesName), title = title)
      chart.domainAxisLabel = xLabel
      chart.rangeAxisLabel = "probability"
      chart.show(dim = (800, 600))
      chart
    }

    def plotXYOn(chart: XYChart)(seriesName: String = "")(implicit ord: Ordering[K], asNum: K => Number): chart.type = {
      chart.plot.getDataset match {
        case seriesList: XYSeriesCollection =>
          seriesList.addSeries(pmf.hist.toSeq.toXYSeries(seriesName))
      }
      chart
    }
  }
}
