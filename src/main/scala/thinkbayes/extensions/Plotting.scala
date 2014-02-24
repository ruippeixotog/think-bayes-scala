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
  implicit def suiteAsPlottable[H](suite: Suite[H, _]) = new Plottable[H] { def values = suite.pmf.hist.toSeq }
  implicit def cdfAsPlottable[K](cdf: Cdf[K]) = new Plottable[K] { def values = cdf.vals }
  implicit def boundedPdfAsPlottable[K](pdf: BoundedPdf) = new Plottable[Double] {
    def values = (pdf.lowerBound to pdf.upperBound by ((pdf.upperBound - pdf.lowerBound) / 10000)).
      map { k => (k, pdf.density(k)) }
  }
}
