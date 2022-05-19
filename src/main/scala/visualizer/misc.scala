package visualizer

import java.awt.image.BufferedImage
import java.awt.Point
import java.awt.Robot
import java.awt.Graphics
import java.awt.Transparency
object misc {
  private def frame = VisualizerApp.frame
  def emptyCursor = frame
    .getToolkit()
    .createCustomCursor(
      frame
        .getGraphicsConfiguration()
        .createCompatibleImage(16, 16, Transparency.BITMASK),
      new Point(0, 0),
      "empty cursor"
    )
  val robot = new Robot()

  //helper functions for time measuring
  def timeNanos() = System.nanoTime()

  def timeBetween(start: Long, end: Long) = (end - start) / 1000000000.0f

  def drawCrosshair(g: Graphics) = {
    val w = GfxMath.screenWidth
    val h = GfxMath.screenHeight
    g.drawLine(w / 2, h / 2 + 10, w / 2, h / 2 - 10)
    g.drawLine(w / 2 + 10, h / 2, w / 2 - 10, h / 2)
  }
}
