package visualizer
import java.awt.Graphics
import scala.collection.parallel.CollectionConverters._
import java.awt.Color
import visualizer.GfxMath._
import scala.concurrent.ExecutionContext
import scala.concurrent._
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
object Rendererer {
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  def createFrames(player: Pos, camera: Pos)(implicit
      ec: ExecutionContext
  ): Vector[(Triangle, Color)] = {
    val start = misc.timeNanos()
    val worldSpaceTriangles =
      VisualizerApp.worldObjects.flatMap(_.worldSpaceTris(player, camera))
    val viewTris = generateViewTriangles(worldSpaceTriangles)
    generateDrawableTriangles(viewTris)
  }

  val frameIterator: Iterator[Future[Vector[(Triangle, Color)]]] =
    new Iterator[Future[Vector[(Triangle, Color)]]] {
      private var current = Future(createFrames(Player.pos, Camera.pos)(ec))
      // private var current2 = Future{Thread.sleep(1);createFrames(Player.pos,Camera.pos)}.flatten
      def hasNext: Boolean = true
      def next(): Future[Vector[(Triangle, Color)]] = {
        val res = current
        current = Future(createFrames(Player.pos, Camera.pos)(ec))
        // current2=Future{Thread.sleep((VisualizerApp.frametimeMulti/1000).toLong);createFrames(Player.pos,Camera.pos)}.flatten
        return res
      }
    }

  def generateViewTriangles(triangles: Vector[Triangle]) = {

    val newTriangles = triangles.par
      .flatMap(tri => {

        val clippedTriangles = calcClipping(tri, zPlane)
        clippedTriangles
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
                .center()
            )
            newTri
          })
          .filter(getNormal(_).z < 0)
      })
    newTriangles.toVector
  }
  def generateDrawableTriangles(
      triangles: Vector[Triangle]
  ): Vector[(Triangle, Color)] = {
    if (VisualizerApp.wireFrame) {
      triangles.map(tri => {
        (tri, Color.WHITE)
      })
    } else
      triangles
        .sortBy(tri => {
          -(tri.pos1.z + tri.pos2.z + tri.pos3.z) / 3
        })
        .map(tri => {
          val normal = getNormal(tri).unit()
          val avgPos = (tri.pos1 + tri.pos2 + tri.pos3) / 3
          // val playerPos =
          // Pos(VisualizerApp.width / 2, VisualizerApp.height / 2, 0)
          // val r = (avgPos).distance(playerPos) // "flashlight"
          val cosBetweenTriandZ = normal.dotProduct(Pos(0, 0, -1))
          // val rSquaredAndConstant = (Math.pow(r, 2) / 10000 + 1)
          val distanceFromZPlane = (avgPos).z / 2000 + 1 // "ambient light"
          val color = (((225 / distanceFromZPlane).toInt + 30) * Math
            .sqrt(cosBetweenTriandZ)).toInt
          (tri, new Color(color, color, color))
        })
  }
  def drawFrame(
      triangles: Vector[(Triangle, Color)],
      g: Graphics,
      wireFrame: Boolean
  ): Unit = {
    if (wireFrame) triangles.foreach(_._1.draw(g))
    else triangles.foreach(n => n._1.draw(g, n._2))
  }
  def drawFramesFuture(
      triangles: Future[Vector[(Triangle, Color)]],
      g: Graphics,
      wireFrame: Boolean
  ): Future[Unit] = {
    val p = Promise[Unit]()
    triangles.onComplete {
      case Success(drawFrame) =>
        p.completeWith(
          Future(
            if (wireFrame) drawFrame.foreach(_._1.draw(g))
            else drawFrame.foreach(n => n._1.draw(g, n._2))
          )
        )
      case Failure(exception) => throw exception
    }
    Await.ready(p.future,Duration.Inf)
  }

}
