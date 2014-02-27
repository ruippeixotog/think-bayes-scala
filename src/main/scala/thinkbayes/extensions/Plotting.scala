package thinkbayes.extensions

import java.awt._
import javax.swing.UIManager
import org.jfree.chart.{ ChartTheme, StandardChartTheme }
import org.jfree.chart.plot.DefaultDrawingSupplier
import org.jfree.chart.renderer.category.StandardBarPainter
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.ui.RectangleInsets
import scalax.chart._
import scalax.chart.Charting._
import thinkbayes._

object Plotting {
  implicit val defaultTheme = darkChartTheme

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

  object ThinkBayesChartTheme {

    final val LightSeriesColors = Array[Paint](
      Color.green.darker(),
      Color.red.darker(),
      Color.blue.darker())

    final val LightSeriesStrokes = Array[Stroke](
      new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

    final val DarkSeriesColors = Array[Paint](
      new Color(160, 255, 160, 128),
      new Color(255, 160, 160, 128),
      new Color(160, 160, 255, 128))

    final val DarkSeriesStrokes = LightSeriesStrokes
  }

  class ThinkBayesChartTheme(name: String) extends StandardChartTheme(name, false) {

    setExtraLargeFont(new Font("Helvetica Neue", Font.BOLD, 20))
    setLargeFont(new Font("Helvetica Neue", Font.BOLD, 14))
    setRegularFont(new Font("Helvetica Neue", Font.PLAIN, 12))
    setSmallFont(new Font("Helvetica Neue", Font.PLAIN, 10))

    setAxisOffset(RectangleInsets.ZERO_INSETS)

    setPlotOutlinePaint(new Color(0, 0, 0, 0))

    setBarPainter(new StandardBarPainter())
    setXYBarPainter(new StandardXYBarPainter())
  }

  lazy val lightChartTheme: ChartTheme = new ThinkBayesChartTheme("think-bayes-light") {

    setChartBackgroundPaint(UIManager.getColor("Panel.background"))
    setPlotBackgroundPaint(Color.white)

    setDomainGridlinePaint(Color.lightGray)
    setRangeGridlinePaint(Color.lightGray)

    setDrawingSupplier(new DefaultDrawingSupplier(
      ThinkBayesChartTheme.LightSeriesColors,
      DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE,
      ThinkBayesChartTheme.LightSeriesStrokes,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE))
  }

  lazy val darkChartTheme: ChartTheme = new ThinkBayesChartTheme("think-bayes-dark") {

    setTitlePaint(Color.white)
    setSubtitlePaint(Color.white)

    setChartBackgroundPaint(new Color(31, 32, 27))
    setPlotBackgroundPaint(new Color(31, 32, 27))

    setDomainGridlinePaint(Color.white)
    setRangeGridlinePaint(Color.white)

    setLegendBackgroundPaint(new Color(31, 32, 27))
    setLegendItemPaint(Color.white)
    setLabelLinkPaint(new Color(0, 0, 0, 0))

    setAxisLabelPaint(Color.white)
    setTickLabelPaint(Color.white)

    setDrawingSupplier(new DefaultDrawingSupplier(
      ThinkBayesChartTheme.DarkSeriesColors,
      DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE,
      ThinkBayesChartTheme.DarkSeriesStrokes,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE,
      DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE))
  }
}
