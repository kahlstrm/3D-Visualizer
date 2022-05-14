package visualizer
import java.awt.Graphics2D
import java.awt.Color._
import java.awt.TexturePaint
import java.awt.image.BufferedImage
import GfxMath._
import java.awt.Color
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import java.awt.Rectangle
import java.awt.Dimension
trait Shapes {
  val position: Pos
  val rotation: Pos
  val poses: Vector[Pos]
  def worldSpacePos = {
    poses.par.map(pos =>
      pos
        .rotate(rotation)
        .translate(position)
        .translate(-Player.pos)
    )
  }
  def worldSpaceTris = {
    triangles.par.map(tri => {
      Triangle(
        tri.pos1
          .rotate(rotation)
          .translate(position)
          .translate(-Player.pos)
          .fpsRotate(0,Camera.x)
          .fpsRotate(Camera.y,0)
          // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0))
          ,
        tri.pos2
          .rotate(rotation)
          .translate(position)
          .translate(-Player.pos)
          .fpsRotate(0,Camera.x)
          .fpsRotate(Camera.y,0) 
          // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0))
          ,
        tri.pos3
          .rotate(rotation)
          .translate(position)
          .translate(-Player.pos)
          .fpsRotate(0,Camera.x)
          .fpsRotate(Camera.y,0)
          // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0))
      )
    })
  }
  val triangles: Vector[Triangle]
  // def draw(g: Graphics2D) = {
  //   val newTriangles = Buffer[Triangle]()
  //   triangles
  //     .foreach(tri => {
  //       val worldSpaceTri = Triangle(
  //         tri.pos1
  //           .rotate(rotation)
  //           .translate(position)
  //           .translate(-Player.pos)
  //           .rotate(-Player.camera.cameraVector()),
  //         tri.pos2
  //           .rotate(rotation)
  //           .translate(position)
  //           .translate(-Player.pos)
  //           .rotate(-Player.camera.cameraVector()),
  //         tri.pos3
  //           .rotate(rotation)
  //           .translate(position)
  //           .translate(-Player.pos)
  //           .rotate(-Player.camera.cameraVector())
  //       )

  //       val clippedTriangles = calcClipping(worldSpaceTri)
  //       clippedTriangles.foreach(n => {
  //         val newTri = Triangle(
  //           n.pos1
  //             .perspective()
  //             .center(),
  //           n.pos2
  //             .perspective()
  //             .center(),
  //           n.pos3
  //             .perspective()
  //             .center()
  //         )
  //         if(getNormal(newTri).z<0) newTriangles+=newTri
  //       })
  //       // Triangle(
  //       //   center(perspective(rotate(translatePos(translatePos(rotate(tri.pos1,rotation),position),-Player.pos),Player.camera))),
  //       //   center(perspective(rotate(translatePos(translatePos(rotate(tri.pos2,rotation),position),-Player.pos),Player.camera))),
  //       //   center(perspective(rotate(translatePos(translatePos(rotate(tri.pos3,rotation),position),-Player.pos),Player.camera)))
  //       // )
  //     })

  //   newTriangles
  //     .sortBy(tri => -(tri.pos1.z + tri.pos2.z + tri.pos3.z) / 3)
  //     .foreach(tri => {
  //       val normal = getNormal(tri).unit()
  //       val avgPos = (tri.pos1 + tri.pos2 + tri.pos3) / 3
  //       val playerPos =
  //         Pos(VisualizerApp.width / 2, VisualizerApp.height / 2, 0)
  //       val r = (avgPos).distance(playerPos)//"flashlight"
  //       val cosBetweenTriandZ =normal.dotProduct(Pos(0,0,-1))
  //       val rSquaredAndConstant=(Math.pow(r, 2) / 10000 + 1)
  //       val distanceFromZPlane=(avgPos).z/2000 + 1 //"ambient light"
  //       val color = (((225/distanceFromZPlane).toInt+30)*Math.sqrt(cosBetweenTriandZ)).toInt
  //       tri.draw(g, new Color(color, color, color))
  //     })

  // }

}

class Triangle(val pos1: Pos, val pos2: Pos, val pos3: Pos) {

  def draw(g: Graphics2D) = {
    g.drawLine(pos1.x.toInt, pos1.y.toInt, pos2.x.toInt, pos2.y.toInt)
    g.drawLine(pos2.x.toInt, pos2.y.toInt, pos3.x.toInt, pos3.y.toInt)
    g.drawLine(pos3.x.toInt, pos3.y.toInt, pos1.x.toInt, pos1.y.toInt)
  }
  def draw(g: Graphics2D, color: Color) = {
    g.setColor(color)
    g.fillPolygon(
      Array[Int](pos1.x.toInt, pos2.x.toInt, pos3.x.toInt),
      Array[Int](pos1.y.toInt, pos2.y.toInt, pos3.y.toInt),
      3
    )
  }
  def draw(g: Graphics2D, texture: TexturePaint) = {
    g.setPaint(texture)
    g.fillPolygon(
      Array[Int](pos1.x.toInt, pos2.x.toInt, pos3.x.toInt),
      Array[Int](pos1.y.toInt, pos2.y.toInt, pos3.y.toInt),
      3
    )
  }
  override def toString(): String =
    pos1.toString + pos2.toString + pos3.toString()
}

object Triangle {
  def apply(pos1: Pos, pos2: Pos, pos3: Pos) = {
    new Triangle(pos1, pos2, pos3)
  }
  def apply(tri: Triangle) = {
    new Triangle(tri.pos1, tri.pos2, tri.pos3)
  }
}
class Object(
    objInfo: (Vector[Pos], Vector[Triangle]),
    val position: Pos,
    val rotation: Pos,
    scale: Double
) extends Shapes {
  val poses = objInfo._1.map(pos => pos * scale)
  val triangles: Vector[Triangle] = objInfo._2.map(tri =>
    new Triangle(tri.pos1 * scale, tri.pos2 * scale, tri.pos3 * scale)
  )
}
class Wall(val position: Pos, val rotation: Pos) extends Shapes {
  val poses = Vector[Pos](
    Pos(-300, -200, -100),
    Pos(300, -200, -100),
    Pos(300, 200, -100),
    Pos(300, 200, 100),
    Pos(-300, 200, 100),
    Pos(-300, -200, 100),
    Pos(300, -200, 100),
    Pos(-300, 200, -100)
  )
  val triangles = Vector[Triangle](
    Triangle(poses(0), poses(7), poses(2)),
    Triangle(poses(0), poses(2), poses(1)),
    Triangle(poses(1), poses(2), poses(3)),
    Triangle(poses(1), poses(3), poses(6)),
    Triangle(poses(6), poses(3), poses(4)),
    Triangle(poses(6), poses(4), poses(5)),
    Triangle(poses(5), poses(4), poses(7)),
    Triangle(poses(5), poses(7), poses(0)),
    Triangle(poses(7), poses(4), poses(3)),
    Triangle(poses(7), poses(3), poses(2)),
    Triangle(poses(6), poses(5), poses(0)),
    Triangle(poses(6), poses(0), poses(1))
  )

  def isInside(pos: Pos) = {
    val worldSpace = this.worldSpacePos
    val bottomCorner = worldSpace(0)
    val topCorner = worldSpace(3)
    def isBetween(a: Double, b: Double, c: Double) =
      Math.min(a, b) - 20 < c && Math.max(a, b) + 20 > c
    isBetween(bottomCorner.x, topCorner.x, pos.x) &&
    isBetween(bottomCorner.y, topCorner.y, pos.y) &&
    isBetween(bottomCorner.z, topCorner.z, pos.z)
  }

}

object renderer {
  def draw(g: Graphics2D, triangles: Array[Triangle]) = {
    val start = System.currentTimeMillis()
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

    newTriangles.seq
      .sortBy(tri => {
        -(tri.pos1.z + tri.pos2.z + tri.pos3.z) / 3
      })
      .foreach(tri => {
        if (VisualizerApp.wireFrame) {
        tri.draw(g)} else {
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
    val end = System.currentTimeMillis()
    VisualizerApp.frametime = (end - start) / 1000.0
  }
}
