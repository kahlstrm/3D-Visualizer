package visualizer
import java.awt.Graphics
import scala.collection.parallel.CollectionConverters._
import java.awt.Color
import java.awt.image.BufferedImage
import visualizer.GfxMath._
import scala.concurrent.ExecutionContext
import scala.concurrent._
import scala.util.Success
import scala.util.Failure
import scala.concurrent.duration.Duration

object Rendererer {
  implicit val ec: ExecutionContext =
    ExecutionContext.global

  def createFrames(player: Pos, camera: Pos2D)(implicit
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
    val viewTris = generateViewTriangles(worldSpaceTriangles)
    val res = generateDrawableTriangles(viewTris)
    res
  }

  val frameIterator: Iterator[Future[Vector[Triangle]]] =
    new Iterator[Future[Vector[Triangle]]] {
      private var current = Future(createFrames(Player.pos, Camera.pos)(ec))
      def hasNext: Boolean = true
      def next(): Future[Vector[Triangle]] = {
        val res = current
        current = Future(createFrames(Player.pos, Camera.pos))
        return res
      }
    }

  def generateViewTriangles(triangles: Vector[Triangle]) = {

    val newTriangles = triangles.par
      .flatMap(tri => {
        val clippedTrianglesWithZ = calcClipping(tri, zPlane, zPlaneNormal)
        clippedTrianglesWithZ
          .map(n => {
            val newTri = Triangle(
              n.poses.map(pos =>
                pos
                  .perspective()
                  .center()
              ),
              n.texPoses,
              n.color
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
          .flatMap(calcClipping(_, Pos(0, 0, 0), Pos(1, 0, 0)))
          .flatMap(calcClipping(_, Pos(screenWidth, 0, 0), Pos(-1, 0, 0)))
          .flatMap(calcClipping(_, Pos(0, 0, 0), Pos(0, 1, 0)))
          .flatMap(calcClipping(_, Pos(0, screenHeight, 0), Pos(0, -1, 0)))
      })
    newTriangles.toVector
  }
  def generateDrawableTriangles(
      triangles: Vector[Triangle]
  ): Vector[Triangle] = {
    VisualizerApp.triangleCount = triangles.size
    if (VisualizerApp.wireFrame) {
      triangles
    } else {
      val sorted = triangles
        .sortBy(tri => {
          -(tri.pos1.z + tri.pos2.z + tri.pos3.z) / 3
        })
      sorted
    }
  }
  def drawFrame(
      triangles: Vector[Triangle],
      g: Graphics,
      wireFrame: Boolean
  ): Unit = {
    if (wireFrame) triangles.foreach(_.draw(g))
    else
      triangles.foreach(tri => {
        if (tri.texPoses != null) println(tri.texPoses.mkString)
        tri.draw(g, tri.color)
      })
  }
  def drawFramesFuture(
      triangles: Future[Vector[Triangle]],
      g: Graphics,
      wireFrame: Boolean
  ): Unit = {
    val p = Promise[Unit]()
    triangles.onComplete {
      case Success(triangles) =>
        p.completeWith(
          Future {
            drawFrame(triangles, g, wireFrame)
          }
        )
      case Failure(exception) => throw exception
    }
    Await.ready(p.future, Duration.Inf)
    return
  }
  def generateFrameImage(triangles: Vector[Triangle]): BufferedImage = {
    val textureImg = VisualizerApp.brickTexture
    val start = misc.timeNanos()
    val img =
      new BufferedImage(screenWidth, screenHeight, BufferedImage.TYPE_INT_ARGB)
    
    for (i <- 0 until screenHeight; j <- 0 until (screenWidth)) {
      // val col = textureImg.getRGB(j % 159, i % 159)
      img.setRGB(j, i, new Color(j * i).getRGB())
    }
    VisualizerApp.othertime = misc.timeBetween(start, misc.timeNanos())
    img
  }

}
