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

    def plotBar(title: String = "", xLabel: String = "")(implicit ord: K => Ordered[K]): CategoryChart = {
      val chart = BarChart(values.sorted.toCategoryDataset, title = title)
      chart.domainAxisLabel = xLabel
      chart.rangeAxisLabel = "probability"
      chart.show(dim = (800, 600))
      chart
    }

    def plotXY(seriesName: String, title: String = "", xLabel: String = "")(implicit asNum: K => Number): XYChart = {
      val chart = XYLineChart(values.toXYSeriesCollection(seriesName), title = title)
      chart.domainAxisLabel = xLabel
      chart.rangeAxisLabel = "probability"
      chart.show(dim = (800, 600))
      chart
    }

    def plotXYOn(chart: XYChart, seriesName: String)(implicit asNum: K => Number): chart.type = {
      chart.plot.getDataset match {
        case seriesList: XYSeriesCollection =>
          seriesList.addSeries(values.toXYSeries(seriesName))
      }
      chart
    }
  }

  implicit def pmfAsPlottable[K](pmf: Pmf[K]) = new Plottable[K] { def values = pmf.hist.toSeq }
  implicit def cdfAsPlottable[K](pmf: Cdf[K]) = new Plottable[K] { def values = pmf.vals }

  trait AutoPlotXY[H, D] extends Suite[H, D] with Plottable[H] {
    def values = hist.toSeq

    private[this] var innerChart = Option.empty[(XYChart, H => Number)]
    private var updateHistLabel: String = ""

    override abstract def plotXY(seriesName: String, title: String = "", xLabel: String = "")(implicit asNum: H => Number): XYChart = {
      val chart = super.plotXY(seriesName, title, xLabel)
      innerChart = Some(chart, asNum)
      chart
    }

    override abstract def update(data: D) = {
      super.update(data)
      updateHistLabel += (if(updateHistLabel.isEmpty) "After " + data else "," + data)
      innerChart.map { case (chart, asNum) =>
        plotXYOn(chart, updateHistLabel)(asNum)
      }
    }
  }
}
