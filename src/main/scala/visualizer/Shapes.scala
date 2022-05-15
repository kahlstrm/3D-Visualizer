package visualizer
import java.awt.TexturePaint
import java.awt.Color
import scala.collection.parallel.CollectionConverters._
import java.awt.Graphics
trait Shapes {
  val position: Pos
  val rotation: Pos
  val poses: Vector[Pos]
  def worldSpacePos(player:Pos) = {
    poses.par.map(pos =>
      pos
        .rotate(rotation)
        .translate(position)
        .translate(-player)
    )
  }
  def worldSpaceTris(player:Pos,camera:Pos) = {
    triangles.par.map(tri => {
      Triangle(
        tri.pos1
          .rotate(rotation)
          .translate(position)
          .translate(-player)
          .fpsRotate(0, camera.x)
          .fpsRotate(camera.y, 0)
        // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0))
        ,
        tri.pos2
          .rotate(rotation)
          .translate(position)
          .translate(-player)
          .fpsRotate(0, camera.x)
          .fpsRotate(camera.y, 0)
        // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0))
        ,
        tri.pos3
          .rotate(rotation)
          .translate(position)
          .translate(-player)
          .fpsRotate(0, camera.x)
          .fpsRotate(camera.y, 0)
        // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0))
      )
    })
  }
  val triangles: Vector[Triangle]
  val bottomCorner: Pos
  val topCorner: Pos

  def isInside(pos: Pos) = {
    val bottomCornerWorld = bottomCorner
      .rotate(rotation)
      .translate(position)
    val topCornerWorld = topCorner
      .rotate(rotation)
      .translate(position)
    def isBetween(a: Double, b: Double, c: Double) =
      Math.min(a, b) - 20 < c && Math.max(a, b) + 20 > c
    isBetween(bottomCornerWorld.x, topCornerWorld.x, pos.x) &&
    isBetween(bottomCornerWorld.y, topCornerWorld.y, pos.y) &&
    isBetween(bottomCornerWorld.z, topCornerWorld.z, pos.z)
  }
}

class Triangle(val pos1: Pos, val pos2: Pos, val pos3: Pos) {
  def draw(g: Graphics) = {
    g.drawLine(pos1.x.toInt, pos1.y.toInt, pos2.x.toInt, pos2.y.toInt)
    g.drawLine(pos2.x.toInt, pos2.y.toInt, pos3.x.toInt, pos3.y.toInt)
    g.drawLine(pos3.x.toInt, pos3.y.toInt, pos1.x.toInt, pos1.y.toInt)
  }
  def draw(g: Graphics, color: Color) = {
    g.setColor(color)
    g.fillPolygon(
      Array[Int](pos1.x.toInt, pos2.x.toInt, pos3.x.toInt),
      Array[Int](pos1.y.toInt, pos2.y.toInt, pos3.y.toInt),
      3
    )
  }
  def draw(g: Graphics, color: Int) = {
    g.setColor(new Color(color, color, color))
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
case class Object(
    objInfo: (Vector[Pos], Vector[Triangle]),
    val position: Pos,
    val rotation: Pos,
    scale: Double
) extends Shapes {
  val poses = objInfo._1.map(pos => pos * scale)
  val triangles: Vector[Triangle] = objInfo._2.map(tri =>
    new Triangle(tri.pos1 * scale, tri.pos2 * scale, tri.pos3 * scale)
  )
  val (bottomCorner, topCorner): (Pos, Pos) = {
    val firstPos = poses.headOption.getOrElse(Pos(0, 0, 0))
    var minX = firstPos.x
    var minY = firstPos.y
    var minZ = firstPos.z

    var maxX = firstPos.x
    var maxY = firstPos.y
    var maxZ = firstPos.z
    poses.foreach(pos => {
      minX = Math.min(pos.x,minX)
      minY = Math.min(pos.y,minY)
      minZ = Math.min(pos.z,minZ)
      maxX = Math.max(pos.x,maxX)
      maxY = Math.max(pos.y,maxY)
      maxZ = Math.max(pos.z,maxZ)
    })
    (Pos(minX, minY, minZ), Pos(maxX, maxY, maxZ))
  }

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
  val bottomCorner = poses(0)
  val topCorner = poses(3)
}
