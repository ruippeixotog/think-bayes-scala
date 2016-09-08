package thinkbayes.extensions.plotting

import scala.swing.Frame
import scala.swing.Swing._
import scala.swing.event.{ WindowClosed, WindowOpened }
import scalax.chart.Chart

import org.jfree.chart.ChartFrame

/**
  * A wrapper for `Chart` instances making it easy for clients to show the chart in a window using Swing, as well as
  * closing it programatically and adding hooks for open and close events.
  *
  * @param chart the chart to show
  */
class ShowControls(val chart: Chart) {
  private[this] var frame = Option.empty[Frame]
  private[this] var onShowHandler = Option.empty[() => Unit]
  private[this] var onHideHandler = Option.empty[() => Unit]

  /**
    * Shows the chart in a window. If the window is already open, this method does nothing.
    */
  def show() = frame match {
    case Some(fr) => fr.open()
    case None => frame = Some(showScalable(chart, chart.title, (1024, 768)))
  }

  /**
    * Closes the chart window. If the window was already closed or it was never created, this method does nothing.
    */
  def hide() = frame.foreach(_.close())

  /**
    * Closes the chart window and releases any resources associated with it. If the window was never created, this
    * method does nothing.
    */
  def dispose() = frame.foreach(_.dispose())

  /**
    * Adds a hook to run each time the window is opened.
    *
    * @param callback the hook to run each time the window is opened.
    */
  def onShow(callback: => Unit) = {
    onShowHandler = Some(() => callback)
  }

  /**
    * Adds a hook to run each time the window is closed.
    *
    * @param callback the hook to run each time the window is closed.
    */
  def onHide(callback: => Unit) = {
    onHideHandler = Some(() => callback)
  }

  private[this] def showScalable(chart: Chart, windowTitle: String, dim: (Int, Int)): Frame = {
    val frame = chart.toFrame(windowTitle)
    val panel = frame.peer.asInstanceOf[ChartFrame].getChartPanel
    panel.setMaximumDrawWidth(Int.MaxValue)
    panel.setMaximumDrawHeight(Int.MaxValue)
    frame.size = dim

    frame.reactions += {
      case WindowOpened(`frame`) => onShowHandler.foreach(_.apply)
      case WindowClosed(`frame`) => onHideHandler.foreach(_.apply)
    }
    frame.visible = true
    frame
  }
}
