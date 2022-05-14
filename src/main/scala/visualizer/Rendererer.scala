package visualizer
import java.awt.Graphics2D
import scala.collection.parallel.CollectionConverters._
import java.awt.Color
import visualizer.GfxMath._
import scala.concurrent.ExecutionContext
import scala.concurrent._
import scala.util.Success
import scala.util.Failure
object Rendererer {
  implicit val ec: ExecutionContext = ExecutionContext.global
  def draw(g: Graphics2D, triangles: Vector[Triangle]) = {

    triangles
      .sortBy(tri => {
        -(tri.pos1.z + tri.pos2.z + tri.pos3.z) / 3
      })
      .foreach(tri => {
        if (VisualizerApp.wireFrame) {
          tri.draw(g)
        } else {
          val normal = getNormal(tri).unit()
          val avgPos = (tri.pos1 + tri.pos2 + tri.pos3) / 3
          val playerPos =
            Pos(VisualizerApp.width / 2, VisualizerApp.height / 2, 0)
          val r = (avgPos).distance(playerPos) // "flashlight"
          val cosBetweenTriandZ = normal.dotProduct(Pos(0, 0, -1))
          val rSquaredAndConstant = (Math.pow(r, 2) / 10000 + 1)
          val distanceFromZPlane = (avgPos).z / 2000 + 1 // "ambient light"
          val color = (((225 / distanceFromZPlane).toInt + 30) * Math
            .sqrt(cosBetweenTriandZ)).toInt

          tri.draw(g, new Color(color, color, color))
        }
      })
  }
  def generateDrawableTriangles(
      triangles: Vector[Triangle]
  ): Vector[(Triangle, Color)] = {
    triangles
      .sortBy(tri => {
        -(tri.pos1.z + tri.pos2.z + tri.pos3.z) / 3
      })
      .map(tri => {
        if (VisualizerApp.wireFrame) {
          (tri, Color.WHITE)
        } else {
          val normal = getNormal(tri).unit()
          val avgPos = (tri.pos1 + tri.pos2 + tri.pos3) / 3
          val playerPos =
            Pos(VisualizerApp.width / 2, VisualizerApp.height / 2, 0)
          val r = (avgPos).distance(playerPos) // "flashlight"
          val cosBetweenTriandZ = normal.dotProduct(Pos(0, 0, -1))
          val rSquaredAndConstant = (Math.pow(r, 2) / 10000 + 1)
          val distanceFromZPlane = (avgPos).z / 2000 + 1 // "ambient light"
          val color = (((225 / distanceFromZPlane).toInt + 30) * Math
            .sqrt(cosBetweenTriandZ)).toInt
          (tri, new Color(color, color, color))
        }
      })
  }
  def generateViewTriangles(triangles: Vector[Triangle]) = {

    val newTriangles = triangles.par
      .flatMap(tri => {

        val clippedTriangles = calcClipping(tri)
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
        // Triangle(
        //   center(perspective(rotate(translatePos(translatePos(rotate(tri.pos1,rotation),position),-Player.pos),Player.camera))),
        //   center(perspective(rotate(translatePos(translatePos(rotate(tri.pos2,rotation),position),-Player.pos),Player.camera))),
        //   center(perspective(rotate(translatePos(translatePos(rotate(tri.pos3,rotation),position),-Player.pos),Player.camera)))
        // )

      })
    newTriangles.toVector
  }
  def createFrames()(implicit
      ec: ExecutionContext
  ): Future[Vector[(Triangle, Color)]] = {
    val start = System.currentTimeMillis()
    val repainter = Promise[Vector[Triangle]]
    val worldSpaceTriangles = Future(
      VisualizerApp.worldObjects.flatMap(_.worldSpaceTris)
    )
    worldSpaceTriangles.onComplete {
      case Success(value) =>
        repainter.completeWith(
          Future(generateViewTriangles(value))
        )
      case Failure(exception) => throw exception
    }
    val repaintF = repainter.future
    val p = Promise[Vector[(Triangle, Color)]]
    repaintF.onComplete({
      case Success(frame) => {
        p.completeWith(
          Future(generateDrawableTriangles(frame))
        )
        val end = System.currentTimeMillis()
        VisualizerApp.frametimeMulti = (end - start) / 1000.0
      }
      case Failure(exception) => throw exception
    })
    p.future
  }

  def drawFrames(
      frames: Future[Vector[(Triangle, Color)]],
      g: Graphics2D,
      wireFrame: Boolean
  ): Future[Unit] = {
    val p = Promise[Unit]()
    frames.onComplete {
      case Success(value) =>
        p.completeWith(Future(value.foreach(n =>{
        if(wireFrame){
          n._1.draw(g)
        }else n._1.draw(g, n._2)
        
        
        } )))
      case Failure(exception) => throw exception
    }
    p.future
  }
}
