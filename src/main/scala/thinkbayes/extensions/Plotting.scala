package thinkbayes.extensions

import org.jfree.chart._
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.XYSeriesCollection
import thinkbayes._
import thinkbayes.extensions.plotting.ThinkBayesChartTheme

import scala.swing.Swing._
import scala.util.Try
import scalax.chart._
import scalax.chart.api._

trait Plotting {
  val defaultTheme = ThinkBayesChartTheme.Dark
  val drawFrame = true

  implicit def mapAsPlottable[K, V](map: Map[K, V])(implicit asNum: Numeric[V]) = new Plottable[K] {
    protected def plotData = map.mapValues(asNum.toDouble).toSeq
  }

  implicit def pmfAsPlottable[K](pmf: Pmf[K]) = new Plottable[K] {
    protected def plotData = pmf.toSeq
    override protected def defaultYLabel = "probability"
  }

  implicit def suiteAsPlottable[H](suite: Suite[H, _]) = new Plottable[H] {
    protected def plotData = suite.pmf.toSeq
    override protected def defaultYLabel = "probability"
  }

  implicit def cdfAsPlottable[K](cdf: Cdf[K]) = new Plottable[K] {
    protected def plotData = cdf.iterator.toSeq
    override protected def defaultYLabel = "probability"
  }

  implicit def boundedPdfAsPlottable[K](pdf: BoundedPdf) = new Plottable[Double] {
    protected def plotData = (pdf.lowerBound to pdf.upperBound by ((pdf.upperBound - pdf.lowerBound) / 10000)).
      map { k => (k, pdf.density(k)) }

    override protected def defaultYLabel = "probability"
  }

  trait Plottable[K] {
    protected def plotData: Seq[(K, Double)]
    protected def defaultXLabel = ""
    protected def defaultYLabel = ""

    /**
      * Plots this object as a category series in a new chart.
      * @param seriesName the unique name of the series
      * @param title the title of the chart
      * @param xLabel the label to draw on the X axis
      * @param yLabel the label to draw on the Y axis
      * @return the newly created chart object.
      */
    def plotBar(seriesName: String, title: String = "", xLabel: String = defaultXLabel, yLabel: String = defaultYLabel)(
      implicit
      ord: K => Ordered[K], theme: ChartTheme = defaultTheme): CategoryChart = {

      plotBarOn(emptyPlotBar(title, xLabel, yLabel), seriesName)
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
          plotData.sorted.foreach { case (k, v) => catDataset.addValue(v, seriesName, k) }
      }
      chart
    }

    /**
      * Plots this object as a XY series in a new chart.
      * @param seriesName the unique name of the series
      * @param title the title of the chart
      * @param xLabel the label to draw on the X axis
      * @param yLabel the label to draw on the Y axis
      * @return the newly created chart object.
      */
    def plotXY(seriesName: String, title: String = "", xLabel: String = defaultXLabel, yLabel: String = defaultYLabel)(
      implicit
      asNum: Numeric[K], theme: ChartTheme = defaultTheme): XYChart = {

      plotXYOn(emptyPlotXY(title, xLabel, yLabel), seriesName)
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
          seriesList.addSeries(plotData.toXYSeries(seriesName))
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
    * @param yLabel the label to draw on the Y axis
    * @return the newly created chart object.
    */
  def emptyPlotBar(title: String = "", xLabel: String = "", yLabel: String = "")(
    implicit
    theme: ChartTheme = defaultTheme): CategoryChart = {

    val chart = BarChart(Seq.empty[(String, Seq[(Int, Double)])], title = title)
    chart.plot.domain.axis.label.text = xLabel
    chart.plot.range.axis.label.text = yLabel

    showScalable(chart, title, (1024, 768))
    chart
  }

  /**
    * Creates an empty chart for plotting XY series.
    * @param title the title of the chart
    * @param xLabel the label to draw on the X axis
    * @param yLabel the label to draw on the Y axis
    * @return the newly created chart object.
    */
  def emptyPlotXY(title: String = "", xLabel: String = "", yLabel: String = "")(
    implicit
    theme: ChartTheme = defaultTheme): XYChart = {

    val chart = XYLineChart(Seq.empty[(String, Seq[(Int, Double)])], title = title)
    chart.plot.domain.axis.label.text = xLabel
    chart.plot.range.axis.label.text = yLabel

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
}

object Plotting extends Plotting {
  object NoSwing extends Plotting {
    override val drawFrame = false
  }
}
