package thinkbayes.extensions

import scala.util.Try
import scalax.chart._
import scalax.chart.api._

import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.XYSeriesCollection
import thinkbayes._
import thinkbayes.extensions.plotting.{ ShowControls, ThinkBayesChartTheme }

trait Plotting {
  val defaultTheme = ThinkBayesChartTheme.Dark

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
      * Plots this object as a category series in a new chart and opens it in a window afterwards.
      * @param seriesName the unique name of the series
      * @param title the title of the chart
      * @param xLabel the label to draw on the X axis
      * @param yLabel the label to draw on the Y axis
      * @return the newly created chart object.
      */
    def showBar(seriesName: String, title: String = "", xLabel: String = defaultXLabel, yLabel: String = defaultYLabel)(
      implicit
      ord: K => Ordered[K], theme: ChartTheme = defaultTheme): CategoryChart = {

      plotBar(seriesName, title, xLabel, yLabel).showScalable()
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
      * Plots this object as a XY series in a new chart and opens it in a window afterwards.
      * @param seriesName the unique name of the series
      * @param title the title of the chart
      * @param xLabel the label to draw on the X axis
      * @param yLabel the label to draw on the Y axis
      * @return the newly created chart object.
      */
    def showXY(seriesName: String, title: String = "", xLabel: String = defaultXLabel, yLabel: String = defaultYLabel)(
      implicit
      asNum: Numeric[K], theme: ChartTheme = defaultTheme): XYChart = {

      plotXY(seriesName, title, xLabel, yLabel).showScalable()
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

  implicit class RichChart[C <: Chart](val chart: C) {

    /**
      * Shows this chart in a new window.
      * @return this chart.
      */
    def showScalable(): chart.type = {
      val controls = new ShowControls(chart)
      controls.onHide(controls.dispose())
      controls.show()
      chart
    }

    /**
      * Returns a `ShowControls` instance for this chart. The returned object contains methods for showing the chart in
      * a window using Swing, as well as closing it programatically and adding hooks for open and close events.
      *
      * @return a `ShowControls` instance for this chart.
      */
    def showControls = new ShowControls(chart)
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

    val chart = BarChart(Seq.empty[(String, Seq[(Int, Double)])])
    chart.title = title
    chart.plot.domain.axis.label.text = xLabel
    chart.plot.range.axis.label.text = yLabel
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

    val chart = XYLineChart(Seq.empty[(String, Seq[(Int, Double)])])
    chart.title = title
    chart.plot.domain.axis.label.text = xLabel
    chart.plot.range.axis.label.text = yLabel
    chart
  }
}

object Plotting extends Plotting
