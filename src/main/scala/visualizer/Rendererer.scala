package visualizer
import java.awt.Graphics
import scala.collection.parallel.CollectionConverters._
import java.awt.Color
import visualizer.GfxMath._
import scala.concurrent.ExecutionContext
import scala.concurrent._
import scala.util.Success
import scala.util.Failure
import scala.concurrent.duration.Duration
object Rendererer {
  implicit val ec: ExecutionContext =
    ExecutionContext.global
  def createFrames(player: Pos, camera: Pos)(implicit
      ec: ExecutionContext
  ): Vector[Triangle] = {
    val worldSpaceTriangles =
      VisualizerApp.worldObjects.flatMap(_.worldSpaceTris(player, camera))
    val start = misc.timeNanos()
    val viewTris = generateViewTriangles(worldSpaceTriangles)
    VisualizerApp.othertime = misc.timeBetween(start, misc.timeNanos())
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
              n.pos1
                .perspective()
                .center(),
              n.pos2
                .perspective()
                .center(),
              n.pos3
                .perspective()
                .center(),
              n.color
            )
            if (newTri.color == null) newTri.color = {
              val col = getColor(newTri)
              new Color(col, col, col)
            }
            newTri
          })
          .filter(getNormal(_).z < 0)
        .flatMap(tri => calcClipping(tri, Pos(0, 0, 0), Pos(1, 0, 0)))
        .flatMap(tri => calcClipping(tri, Pos(screenWidth, 0, 0), Pos(-1, 0, 0)))
        .flatMap(tri => calcClipping(tri, Pos(0, 0, 0), Pos(0, 1, 0)))
        .flatMap(tri => calcClipping(tri, Pos(0, screenHeight, 0), Pos(0, -1, 0)))
      })
    newTriangles.toVector
  }
  def generateDrawableTriangles(
      triangles: Vector[Triangle]
  ): Vector[Triangle] = {
    VisualizerApp.triangleCount = triangles.size
    if (VisualizerApp.wireFrame) {
      triangles
      // .flatMap(tri => calcClipping(tri, Pos(0, 0, 0), Pos(1, 0, 0)))
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
    else triangles.foreach(tri => tri.draw(g, tri.color))
  }
  def drawFramesFuture(
      triangles: Future[Vector[Triangle]],
      g: Graphics,
      wireFrame: Boolean
  ): Unit = {
    val p = Promise[Unit]()
    triangles.onComplete {
      case Success(drawFrame) =>
        p.completeWith(
          Future {
            if (wireFrame) drawFrame.foreach(_.draw(g))
            else drawFrame.foreach { case tri => tri.draw(g, tri.color) }
          }
        )
      case Failure(exception) => throw exception
    }
    Await.ready(p.future, Duration.Inf)
    return
  }

}
