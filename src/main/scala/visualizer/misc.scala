package visualizer

import java.awt.image.BufferedImage
import java.awt.Toolkit
import java.awt.Point
import java.awt.Robot
import java.awt.Graphics2D

object misc {
  val emptyCursor = Toolkit
    .getDefaultToolkit()
    .createCustomCursor(
      new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB),
      new Point(0, 0),
      "empty cursor"
    )
  val robot = new Robot()

  /** Class for measuring time Wall time
    */
  def timeMillis()=System.currentTimeMillis()

  def timeBetween(start:Long,end:Long)=(end-start)/1000.0
  

  def drawCrosshair(g: Graphics2D) = {
    val w = VisualizerApp.width
    val h = VisualizerApp.height
    g.drawLine(w / 2, h / 2 + 10, w / 2, h / 2 - 10)
    g.drawLine(w / 2 + 10, h / 2, w / 2 - 10, h / 2)
  }
}
