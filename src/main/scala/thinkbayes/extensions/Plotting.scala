package thinkbayes.extensions

import java.awt._
import javax.swing.UIManager
import org.jfree.chart._
import org.jfree.chart.block.LineBorder
import org.jfree.chart.title.{ LegendTitle, Title }
import org.jfree.chart.plot.DefaultDrawingSupplier
import org.jfree.chart.renderer.category.{ BarRenderer, CategoryItemRenderer, StandardBarPainter }
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.ui.RectangleInsets
import scala.swing.Swing._
import scalax.chart._
import scalax.chart.api._
import thinkbayes._

trait Plotting {
  val defaultTheme = darkChartTheme
  val drawFrame = true

  trait Plottable[K] {
    def values: Seq[(K, Double)]

    def plotBar(seriesName: String, title: String = "", xLabel: String = "")(
      implicit ord: K => Ordered[K], theme: ChartTheme = defaultTheme): CategoryChart = {

      val chart = BarChart(Seq(seriesName -> values.sorted), title = title)
      chart.plot.domain.axis.label = xLabel
      chart.plot.range.axis.label = "probability"

      showScalable(chart, title, (1024, 768))
      chart
    }

    def plotBarOn(chart: CategoryChart, seriesName: String)(
      implicit ord: K => Ordered[K], theme: ChartTheme = defaultTheme): chart.type = {

      chart.plot.getDataset match {
        case catDataset: DefaultCategoryDataset =>
          values.sorted.foreach { case (k, prob) => catDataset.addValue(prob, seriesName, k) }
      }
      chart
    }

    def plotXY(seriesName: String, title: String = "", xLabel: String = "")(
      implicit asNum: Numeric[K], theme: ChartTheme = defaultTheme): XYChart = {

      val chart = XYLineChart(Seq(seriesName -> values), title = title)
      chart.plot.domain.axis.label = xLabel
      chart.plot.range.axis.label = "probability"

      showScalable(chart, title, (1024, 768))
      chart
    }

    def plotXYOn(chart: XYChart, seriesName: String)(
      implicit asNum: Numeric[K], theme: ChartTheme = defaultTheme): chart.type = {

      chart.plot.getDataset match {
        case seriesList: XYSeriesCollection =>
          seriesList.addSeries(values.toXYSeries(seriesName))
      }
      chart
    }

    private[this] def showScalable(chart: Chart, windowTitle: String, dim: (Int, Int)) {
      if (drawFrame) {
        val frame = chart.toFrame(windowTitle)
        val panel = frame.peer.asInstanceOf[ChartFrame].getChartPanel
        panel.setMaximumDrawWidth(Int.MaxValue)
        panel.setMaximumDrawHeight(Int.MaxValue)
        frame.size = dim
        frame.visible = true
      }
    }
  }

  implicit def mapAsPlottable[K](map: Map[K, Double]) = new Plottable[K] { def values = map.toSeq }
  implicit def suiteAsPlottable[H](suite: Suite[H, _]) = new Plottable[H] { def values = suite.pmf.toSeq }
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
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    setExtraLargeFont(new Font("Helvetica Neue", Font.BOLD, 20))
    setLargeFont(new Font("Helvetica Neue", Font.BOLD, 14))
    setRegularFont(new Font("Helvetica Neue", Font.PLAIN, 12))
    setSmallFont(new Font("Helvetica Neue", Font.PLAIN, 10))

    setAxisOffset(RectangleInsets.ZERO_INSETS)

    setPlotOutlinePaint(new Color(0, 0, 0, 0))

    setBarPainter(new StandardBarPainter())
    setXYBarPainter(new StandardXYBarPainter())

    override def applyToCategoryItemRenderer(renderer: CategoryItemRenderer) {
      super.applyToCategoryItemRenderer(renderer)
      renderer match {
        case br: BarRenderer => br.setItemMargin(0.0)
      }
    }
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

    override def applyToTitle(title: Title) {
      super.applyToTitle(title)
      title match {
        case lt: LegendTitle =>
          lt.setFrame(new LineBorder(new Color(0, 0, 0, 0),
            new BasicStroke(),
            RectangleInsets.ZERO_INSETS))
      }
    }
  }
}

object Plotting extends Plotting {
  object NoSwing extends Plotting {
    override val drawFrame = false
  }
}
