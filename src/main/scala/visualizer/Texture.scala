package visualizer

import java.awt.image.BufferedImage

// simple texture class that provides color information
class Texture(image: BufferedImage) {
  private lazy val texturePixels = image.getData().getDataBuffer()
  private val width = image.getWidth()
  private val height = image.getHeight()

  // returns colors
  def getColorPixel(x: Int, y: Int): Int = {
    return texturePixels.getElem(x%width + y%height * width)
  }
  def getColor(x: Float, y: Float): Int = {
    val xPixel = Math.max(0, Math.ceil(x * width).toInt - 1)
    val yPixel = Math.max(0, Math.ceil(y * height).toInt - 1)
    return getColorPixel(xPixel, yPixel)
  }
}
object Texture{
  def apply(imageSource:String):Texture={
    val image =FileLoader.loadTexture(imageSource)
    if(image!=null) new Texture(image) else null
  }
}