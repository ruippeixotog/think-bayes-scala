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
import scala.util.Try
import scalax.chart._
import scalax.chart.api._
import thinkbayes._

trait Plotting {
  val defaultTheme = darkChartTheme
  val drawFrame = true

  implicit def mapAsPlottable[K](map: Map[K, Double]) = new Plottable[K] { def values = map.toSeq }
  implicit def suiteAsPlottable[H](suite: Suite[H, _]) = new Plottable[H] { def values = suite.pmf.toSeq }
  implicit def cdfAsPlottable[K](cdf: Cdf[K]) = new Plottable[K] { def values = cdf.iterator.toSeq }
  implicit def boundedPdfAsPlottable[K](pdf: BoundedPdf) = new Plottable[Double] {
    def values = (pdf.lowerBound to pdf.upperBound by ((pdf.upperBound - pdf.lowerBound) / 10000)).
      map { k => (k, pdf.density(k)) }
  }

  trait Plottable[K] {
    def values: Seq[(K, Double)]

    /**
     * Plots this object as a category series in a new chart.
     * @param seriesName the unique name of the series
     * @param title the title of the chart
     * @param xLabel the label to draw on the X axis
     * @return the newly created chart object.
     */
    def plotBar(seriesName: String, title: String = "", xLabel: String = "")(
      implicit ord: K => Ordered[K], theme: ChartTheme = defaultTheme): CategoryChart = {

      plotBarOn(emptyPlotBar(title, xLabel), seriesName)
    }

    /**
     * Plots this object as a category series in the provided chart. If the given series name was used before, the data
     * of that series is replaced with the new data.
     * @param chart the category chart to plot this object on
     * @param seriesName the unique name of the series
     * @return the provided chart object.
     */
    def plotBarOn(chart: CategoryChart, seriesName: String)(implicit ord: K => Ordered[K]): chart.type = {
      chart.plot.getDataset match {
        case catDataset: DefaultCategoryDataset =>
          Try(catDataset.removeRow(seriesName))
          values.sorted.foreach { case (k, prob) => catDataset.addValue(prob, seriesName, k) }
      }
      chart
    }

    /**
     * Plots this object as a XY series in a new chart.
     * @param seriesName the unique name of the series
     * @param title the title of the chart
     * @param xLabel the label to draw on the X axis
     * @return the newly created chart object.
     */
    def plotXY(seriesName: String, title: String = "", xLabel: String = "")(
      implicit asNum: Numeric[K], theme: ChartTheme = defaultTheme): XYChart = {

      plotXYOn(emptyPlotXY(title, xLabel), seriesName)
    }

    /**
     * Plots this object as a XY series in the provided chart. If the given series name was used before, the data of
     * that series is replaced with the new data.
     * @param chart the XY chart to plot this object on
     * @param seriesName the unique name of the series
     * @return the provided chart object.
     */
    def plotXYOn[A <: XYChart](chart: A, seriesName: String)(implicit asNum: Numeric[K]): chart.type = {
      chart.plot.getDataset match {
        case seriesList: XYSeriesCollection =>
          Try(seriesList.removeSeries(seriesList.getSeriesIndex(seriesName)))
          seriesList.addSeries(values.toXYSeries(seriesName))
      }
      chart
    }
  }

  implicit class RichCategoryChart(val chart: CategoryChart) {

    /**
     * Plots a category series in this chart. If the given series name was used before, the data of that series is
     * replaced with the new data.
     * @param plottable the plottable object to draw
     * @param seriesName the unique name of the series
     * @tparam K the type of the keys
     * @return this chart.
     */
    def plotBar[K](plottable: Plottable[K], seriesName: String)(implicit ord: K => Ordered[K]): chart.type =
      plottable.plotBarOn(chart, seriesName)

    /**
     * Removes a previously drawn series from this category chart.
     * @param seriesName the unique name of the series
     * @return this chart.
     */
    def removeSeries(seriesName: String): chart.type = chart.plot.getDataset match {
      case catDataset: DefaultCategoryDataset => catDataset.removeRow(seriesName); chart
    }
  }

  implicit class RichXYChart(val chart: XYChart) {

    /**
     * Plots a XY series in this chart. If the given series name was used before, the data of that series is replaced
     * with the new data.
     * @param plottable the plottable object to draw
     * @param seriesName the unique name of the series
     * @tparam K the type of the keys
     * @return this chart.
     */
    def plotXY[K](plottable: Plottable[K], seriesName: String)(implicit asNum: Numeric[K]): chart.type =
      plottable.plotXYOn(chart, seriesName)

    /**
     * Removes a previously drawn series from this chart.
     * @param seriesName the unique name of the series
     * @return this chart.
     */
    def removeSeries(seriesName: String): chart.type = chart.plot.getDataset match {
      case seriesList: XYSeriesCollection => seriesList.removeSeries(seriesList.getSeriesIndex(seriesName)); chart
    }
  }

  /**
   * Creates an empty chart for plotting category series.
   * @param title the title of the chart
   * @param xLabel the label to draw on the X axis
   * @return the newly created chart object.
   */
  def emptyPlotBar(title: String = "", xLabel: String = "")(implicit theme: ChartTheme = defaultTheme): CategoryChart = {
    val chart = BarChart(Seq.empty[(String, Seq[(Int, Double)])], title = title)
    chart.plot.domain.axis.label = xLabel
    chart.plot.range.axis.label = "probability"

    showScalable(chart, title, (1024, 768))
    chart
  }

  /**
   * Creates an empty chart for plotting XY series.
   * @param title the title of the chart
   * @param xLabel the label to draw on the X axis
   * @return the newly created chart object.
   */
  def emptyPlotXY(title: String = "", xLabel: String = "")(implicit theme: ChartTheme = defaultTheme): XYChart = {
    val chart = XYLineChart(Seq.empty[(String, Seq[(Int, Double)])], title = title)
    chart.plot.domain.axis.label = xLabel
    chart.plot.range.axis.label = "probability"

    showScalable(chart, title, (1024, 768))
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
