package thinkbayes.extensions.plotting

import java.awt.{ Stroke, BasicStroke, Color, Font }
import javax.swing.UIManager

import org.jfree.chart.StandardChartTheme
import org.jfree.chart.block.LineBorder
import org.jfree.chart.plot.DefaultDrawingSupplier
import org.jfree.chart.renderer.category.{ BarRenderer, CategoryItemRenderer, StandardBarPainter }
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.chart.title.{ LegendTitle, Title }
import org.jfree.ui.RectangleInsets

import scalax.chart.api._

class ThinkBayesChartTheme(name: String) extends StandardChartTheme(name, false) {

  def strokeSequence: Array[Stroke] = Array(
    new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

  def paintSequence: Array[Paint] =
    DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  setExtraLargeFont(new Font("Helvetica Neue", Font.BOLD, 20))
  setLargeFont(new Font("Helvetica Neue", Font.BOLD, 14))
  setRegularFont(new Font("Helvetica Neue", Font.PLAIN, 12))
  setSmallFont(new Font("Helvetica Neue", Font.PLAIN, 10))

  setAxisOffset(RectangleInsets.ZERO_INSETS)

  setPlotOutlinePaint(ThinkBayesChartTheme.Transparent)

  setBarPainter(new StandardBarPainter())
  setXYBarPainter(new StandardXYBarPainter())

  setLegendBackgroundPaint(ThinkBayesChartTheme.Transparent)
  setLabelLinkPaint(ThinkBayesChartTheme.Transparent)

  setDrawingSupplier(new DefaultDrawingSupplier(
    paintSequence,
    DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE,
    DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE,
    strokeSequence,
    DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE,
    DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE))

  override def applyToCategoryItemRenderer(renderer: CategoryItemRenderer) {
    super.applyToCategoryItemRenderer(renderer)
    renderer match {
      case br: BarRenderer => br.setItemMargin(0.0)
    }
  }

  override def applyToTitle(title: Title) {
    super.applyToTitle(title)
    title match {
      case lt: LegendTitle =>
        lt.setFrame(new LineBorder(
          new Color(0, 0, 0, 0),
          new BasicStroke(),
          RectangleInsets.ZERO_INSETS))
    }
  }
}

object ThinkBayesChartTheme {
  final val Transparent = new Color(0, 0, 0, 0)

  object Light extends ThinkBayesChartTheme("think-bayes-light") {

    override def paintSequence = Array(
      new Color(236, 93, 87),
      new Color(112, 191, 65),
      new Color(81, 167, 249))

    setChartBackgroundPaint(Color.white)
    setPlotBackgroundPaint(Color.white)

    setDomainGridlinePaint(Color.lightGray)
    setRangeGridlinePaint(Color.lightGray)
  }

  object Dark extends ThinkBayesChartTheme("think-bayes-dark") {

    override def paintSequence = Array(
      new Color(160, 255, 160, 128),
      new Color(255, 160, 160, 128),
      new Color(160, 160, 255, 128))

    setTitlePaint(Color.white)
    setSubtitlePaint(Color.white)

    setChartBackgroundPaint(new Color(31, 32, 27))
    setPlotBackgroundPaint(new Color(31, 32, 27))

    setDomainGridlinePaint(Color.white)
    setRangeGridlinePaint(Color.white)

    setLegendItemPaint(Color.white)

    setAxisLabelPaint(Color.white)
    setTickLabelPaint(Color.white)
  }
}
