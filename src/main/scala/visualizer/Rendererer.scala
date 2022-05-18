package visualizer
import java.awt.Graphics
import scala.collection.parallel.CollectionConverters._
import java.awt.Color
import java.awt.image.BufferedImage
import visualizer.GfxMath._
import java.awt.image.DataBuffer
import scala.concurrent.ExecutionContext
object Rendererer {
  implicit val ec: ExecutionContext =
    ExecutionContext.global

  def createFrames(player: Pos, camera: Pos)(implicit
      ec: ExecutionContext
  ): Vector[Triangle] = {
    val worldSpaceTriangles =
      VisualizerApp.worldObjects.flatMap(
        _.worldSpaceTris(player, camera)
          .filter(tri => {
            val avgPos = ((tri.pos1 + tri.pos2 + tri.pos3) / 3).length
            avgPos < VisualizerApp.renderDistance
          })
      )
    val triangles = generateViewTriangles(worldSpaceTriangles)
    VisualizerApp.triangleCount = triangles.size
    // val res = generateDrawableTriangles(viewTris)
    triangles
  }

  def generateViewTriangles(triangles: Vector[Triangle]) = {

    val newTriangles = triangles.par
      .flatMap(tri => {
        val clippedTrianglesWithZ = calcClipping(tri, zPlane, zPlaneNormal)
        clippedTrianglesWithZ
          .map(n => {
            val newTri =
              Triangle(
                n.poses.map(pos =>
                  pos
                    .perspective()
                    .center()
                ),
                Array[Pos](
                  n.texPos1.perspectiveTexture(n.pos1.z),
                  n.texPos2.perspectiveTexture(n.pos2.z),
                  n.texPos3.perspectiveTexture(n.pos3.z)
                ),
                n.color,
                n.texture
              )
            if (newTri.color == null) newTri.color = {
              val col = getColor(newTri)
              new Color(col, col, col)
            }
            newTri
          })
          .filter(getNormal(_).z < 0)
          // calculate clippings for the sides of the screen, which is represented by a plane with point on the plane,
          // and with the normal pointing towards the screen
          .flatMap(calcClipping(_, Pos(0, 30, 0), Pos(1, 0, 0)))
          .flatMap(calcClipping(_, Pos(screenWidth - 9, 0, 0), Pos(-1, 0, 0)))
          .flatMap(calcClipping(_, Pos(0, 9, 0), Pos(0, 1, 0)))
          .flatMap(calcClipping(_, Pos(0, screenHeight - 9, 0), Pos(0, -1, 0)))
      })
    newTriangles.toVector
  }

// draw
  def triangleTextureDraw(
      tri: Triangle,
      pixels: DataBuffer,
      zBuffer: DataBuffer
  ): Unit = {
    val texture = tri.texture
    val yDescTri = tri.sortbyYAsc

    // rename the position variables from triangles yDescTri
    val (x1, y1, x2, y2, x3, y3) = (
      yDescTri.pos1.x.toInt,
      yDescTri.pos1.y.toInt,
      yDescTri.pos2.x.toInt,
      yDescTri.pos2.y.toInt,
      yDescTri.pos3.x.toInt,
      yDescTri.pos3.y.toInt
    )
    // rename the texture variables from triangles yDescTri
    val (tx1, ty1, tz1, tx2, ty2, tz2, tx3, ty3, tz3) = (
      yDescTri.texPos1.x,
      yDescTri.texPos1.y,
      yDescTri.texPos1.z,
      yDescTri.texPos2.x,
      yDescTri.texPos2.y,
      yDescTri.texPos2.z,
      yDescTri.texPos3.x,
      yDescTri.texPos3.y,
      yDescTri.texPos3.z
    )
    var dy1 = y2 - y1
    var dx1 = x2 - x1

    var dv1 = ty2 - ty1
    var du1 = tx2 - tx1
    var dw1 = tz2 - tz1

    val dy2 = y3 - y1
    val dx2 = x3 - x1

    val dv2 = ty3 - ty1
    var du2 = tx3 - tx1
    var dw2 = tz3 - tz1

    var xLeft = 0.0
    var xRight = 0.0
    var du1_step = 0.0
    var dv1_step = 0.0
    var dw1_step = 0.0
    var du2_step = 0.0
    var dv2_step = 0.0
    var dw2_step = 0.0
    if (dy1 != 0) {
      xLeft = dx1 / Math.abs(dy1).toDouble
      du1_step = du1 / Math.abs(dy1)
      dv1_step = dv1 / Math.abs(dy1)
      dw1_step = dw1 / Math.abs(dy1)
    }
    if (dy2 != 0) {
      xRight = dx2 / Math.abs(dy2).toDouble
      du2_step = du2 / Math.abs(dy2)
      dv2_step = dv2 / Math.abs(dy2)
      dw2_step = dw2 / Math.abs(dy2)
    }
    if (dy1 != 0) {
      var j = y1
      while (j <= y2) {
        var xL = (x1 + (j - y1) * xLeft).toInt
        var xR = (x1 + (j - y1) * xRight).toInt

        var texS = tx1 + (j - y1) * du1_step
        var teyS = ty1 + (j - y1) * dv1_step
        var tezS = tz1 + (j - y1) * dw1_step

        var texE = tx1 + (j - y1) * du2_step
        var teyE = ty1 + (j - y1) * dv2_step
        var tezE = tz1 + (j - y1) * dw2_step

        if (xL > xR) {
          val (temp1, temp2, temp3, temp4) = (xL, texS, teyS, tezS)
          xL = xR
          texS = texE
          teyS = teyE
          tezS = tezE
          xR = temp1
          texE = temp2
          teyE = temp3
          tezE = temp4
        }
        var tLocU = texS
        var tLocV = teyS
        var tLocW = tezS

        val texStep = 1.0 / (xR - xL)
        var t = 0.0
        var i = xL
        while (i < xR) {
          tLocU = (1 - t) * texS + t * texE
          tLocV = (1 - t) * teyS + t * teyE
          tLocW = (1 - t) * tezS + t * tezE
          val screenLoc = i + j * screenWidth
          if (tLocW > zBuffer.getElemDouble(screenLoc)) {
            val col =
              if (texture != null)
                texture.getColor(tLocU / tLocW, tLocV / tLocW)
              else tri.color.getRGB()
            pixels.setElem(screenLoc, col)
            zBuffer.setElemDouble(screenLoc, tLocW)
          }
          t += texStep
          i += 1
        }

        j += 1
      }
    }
    dy1 = y3 - y2
    dx1 = x3 - x2
    dv1 = ty3 - ty2
    du1 = tx3 - tx2
    dw1 = tz3 - tz2;
    if (dy1 != 0) {
      xLeft = dx1 / Math.abs(dy1).toDouble
      du1_step = du1 / Math.abs(dy1).toDouble
      dv1_step = dv1 / Math.abs(dy1).toDouble
      dw1_step = dw1 / Math.abs(dy1).toDouble
    }
    if (dy2 != 0) {
      xRight = dx2 / Math.abs(dy2).toDouble
    }
    if (dy1 != 0) {
      var j = y2
      while (j <= y3) {
        var xL = (x2 + (j - y2) * xLeft).toInt
        var xR = (x1 + (j - y1) * xRight).toInt

        var texS = tx2 + (j - y2) * du1_step
        var teyS = ty2 + (j - y2) * dv1_step
        var tezS = tz2 + (j - y2) * dw1_step

        var texE = tx1 + (j - y1) * du2_step
        var teyE = ty1 + (j - y1) * dv2_step
        var tezE = tz1 + (j - y1) * dw2_step

        if (xL > xR) {
          val (temp1, temp2, temp3, temp4) = (xL, texS, teyS, tezS)
          xL = xR
          texS = texE
          teyS = teyE
          tezS = tezE
          xR = temp1
          texE = temp2
          teyE = temp3
          tezE = temp4
        }
        var tLocU = texS
        var tLocV = teyS
        var tLocW = tezS

        val texStep = 1.0 / (xR - xL)
        var t = 0.0
        var i = xL
        while (i < xR) {
          tLocU = (1 - t) * texS + t * texE
          tLocV = (1 - t) * teyS + t * teyE
          tLocW = (1 - t) * tezS + t * tezE
          val screenLoc = i + j * screenWidth
          if (tLocW > zBuffer.getElemDouble(screenLoc)) {
            val col =
              if (texture != null)
                texture.getColor(tLocU / tLocW, tLocV / tLocW)
              else tri.color.getRGB()
            pixels.setElem(screenLoc, col)
            zBuffer.setElemDouble(screenLoc, tLocW)
          }
          t += texStep
          i += 1
        }

        j += 1
      }
    }
  }

  // draws the image to be put to the framebuffer
  def generateFrameImage(
      triangles: Vector[Triangle],
      zBuffer: DataBuffer,
      image: BufferedImage
  ): BufferedImage = {
    val imagePixels = image.getRaster().getDataBuffer()
    val start = misc.timeNanos()
    triangles.foreach(tri => {
      triangleTextureDraw(tri, imagePixels, zBuffer)
    })
    VisualizerApp.othertime = misc.timeBetween(start, misc.timeNanos())
    image
  }
  // old method which is still used for drawing wireframes which slight remodeling
  def drawFrame(
      triangles: Vector[Triangle],
      g: Graphics
      // wireFrame: Boolean
  ): Unit = {
    /*if (wireFrame)*/
    triangles.foreach(_.draw(g))
    // else
    //   triangles.foreach(tri => {
    //     if (tri.color != null) tri.draw(g, tri.color)
    //   })
  }

  // Obsolete implementations after adding imagebased drawing, just for memories. Use not recommended!

  // redundant since zBuffer exists
  // def generateDrawableTriangles(
  //     triangles: Vector[Triangle]
  // ): Vector[Triangle] = {
  //   VisualizerApp.triangleCount = triangles.size
  //   if (VisualizerApp.wireFrame) {
  //     triangles
  //   } else {
  //     triangles
  //       .sortBy(tri => {
  //         -(tri.pos1.z + tri.pos2.z + tri.pos3.z) / 3
  //       })

  //   }
  // }

  // def drawFramesFuture(
  //     triangles: Future[Vector[Triangle]],
  //     g: Graphics,
  //     wireFrame: Boolean
  // ): Unit = {
  //   val p = Promise[Unit]()
  //   triangles.onComplete {
  //     case Success(triangles) =>
  //       p.completeWith(
  //         Future {
  //           drawFrame(triangles, g, wireFrame)
  //         }
  //       )
  //     case Failure(exception) => throw exception
  //   }
  //   Await.ready(p.future, Duration.Inf)
  //   return
  // }

  // def generateFrameFuture(
  //     triangles: Future[Vector[Triangle]],
  //     zBuffer: DataBuffer,
  //     image: BufferedImage,
  //     g: Graphics
  // ): Unit = {
  //   val p = Promise[Unit]()
  //   triangles.onComplete {
  //     case Success(triangles) =>
  //       p.completeWith(
  //         Future {
  //           g.drawImage(
  //             generateFrameImage(
  //               createFrames(Player.pos, Camera.pos),
  //               zBuffer,
  //               image
  //             ),
  //             0,
  //             0,
  //             image.getWidth(),
  //             image.getHeight(),
  //             null
  //           )
  //         }
  //       )
  //     case Failure(exception) => throw exception
  //   }
  //   Await.ready(p.future, Duration.Inf)
  //   return
  // }
  // def frameIterator: Iterator[Future[Vector[Triangle]]] =
  //   new Iterator[Future[Vector[Triangle]]] {
  //     private var current = Future(createFrames(Player.pos, Camera.pos)(ec))
  //     def hasNext: Boolean = true
  //     def next(): Future[Vector[Triangle]] = {
  //       val res = current
  //       current = Future(createFrames(Player.pos, Camera.pos))
  //       return res
  //     }
  //   }
}
