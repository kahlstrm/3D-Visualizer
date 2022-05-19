package visualizer

import java.awt.image.BufferedImage

// simple texture class that provides color information
case class Texture(image: BufferedImage) {
  private val texturePixels = image.getData().getDataBuffer()

  // returns colors
  def getColorPixel(x: Int, y: Int): Int = {
    if (x >= width || x < 0 || y >= height || x < 0) {
      println(f"x:$x y:$y out of range")
      return 0
    }
    return texturePixels.getElem(x + y * width)
  }
  def getColor(x: Float, y: Float): Int = {
    if (x > 1 || x < 0 || y > 1 || y < 0) {
      // println(f"x:$x y:$y out of range")
      return 0
    }
    val xPixel = Math.max(0, Math.ceil(x * width).toInt - 1)
    val yPixel = Math.max(0, Math.ceil(y * height).toInt - 1)
    return getColorPixel(xPixel, yPixel)
  }
  private val width = image.getWidth()
  private val height = image.getHeight()
}
