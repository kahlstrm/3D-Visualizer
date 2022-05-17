package visualizer
import java.awt.TexturePaint
import java.awt.Color
import scala.collection.parallel.CollectionConverters._
import java.awt.Graphics
import java.awt.image.BufferedImage
trait Shapes {
  val position: Pos
  val rotation: Pos
  val poses: Vector[Pos]
  def worldSpacePos(player: Pos) = {
    poses.par.map(pos =>
      pos
        .translate(-player)
    )
  }
  def worldSpaceTris(player: Pos, camera: Pos2D) = {
    triangles.par.map(tri => {
      Triangle(
        tri.poses.map(n =>
          n
            .translate(-player)
            .fpsRotate(0, camera.x)
            .fpsRotate(camera.y, 0)
        ),
        tri.texPoses,
        // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0)),
        tri.color
      )
    })
  }
  val triangles: Vector[Triangle]
  val bottomCornerWorld: Pos
  val topCornerWorld: Pos

  def isInside(pos: Pos) = {
    def isBetween(a: Double, b: Double, c: Double) =
      Math.min(a, b) - 20 < c && Math.max(a, b) + 20 > c
    isBetween(bottomCornerWorld.x, topCornerWorld.x, pos.x) &&
    isBetween(bottomCornerWorld.y, topCornerWorld.y, pos.y) &&
    isBetween(bottomCornerWorld.z, topCornerWorld.z, pos.z)
  }
  val texture: BufferedImage
}

class Triangle(
    val poses: Array[Pos],
    val texPoses: Array[Pos2D] = null,
    var color: Color = null
) {
  def pos1 = poses(0)
  def pos2 = poses(1)
  def pos3 = poses(2)
  def texPos1 = texPoses(0)
  def texPos2 = texPoses(1)
  def texPos3 = texPoses(2)
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
      poses.map(_.x.toInt),
      poses.map(_.y.toInt),
      3
    )
  }
  def foreach[U](f: Pos => U): Unit = {
    f(pos1)
    f(pos2)
    f(pos3)
  }
  override def toString(): String =
    pos1.toString + pos2.toString + pos3.toString()
}

object Triangle {
  def apply(poses: (Pos, Pos, Pos)) = {
    new Triangle(Array(poses._1, poses._2, poses._3))
  }
  def apply(poses: Array[Pos]) = {
    new Triangle(poses)
  }
  def apply(pos1: Pos, pos2: Pos, pos3: Pos, texPoses: Array[Pos2D]) = {
    new Triangle(Array(pos1, pos2, pos3), texPoses)
  }
  def apply(
      pos1: Pos,
      pos2: Pos,
      pos3: Pos,
      texPoses: Array[Pos2D],
      col: Color
  ) = {
    new Triangle(Array(pos1, pos2, pos3), texPoses, col)
  }
  def apply(poses: (Pos, Pos, Pos), col: Color) = {
    new Triangle(Array(poses._1, poses._2, poses._3), color = col)
  }
  def apply(poses: (Pos, Pos, Pos), texPoses: (Pos2D, Pos2D, Pos2D)) = {
    new Triangle(
      Array(poses._1, poses._2, poses._3),
      texPoses = Array(texPoses._1, texPoses._2, texPoses._3)
    )
  }
  def apply(pos1: Pos, pos2: Pos, pos3: Pos) = {
    new Triangle(
      Array(pos1, pos2, pos3)
    )
  }
  def apply(positions: Array[Pos], texPositions: Array[Pos2D], col: Color) = {
    new Triangle(
      positions,
      texPositions,
      col
    )
  }

}
case class Object(
    objInfo: (Vector[Pos], Vector[Triangle]),
    val position: Pos,
    val rotation: Pos,
    scale: Double
) extends Shapes {
  val poses = objInfo._1.map(pos =>
    (pos * scale)
      .rotate(rotation)
      .translate(position)
  )
  val triangles: Vector[Triangle] = objInfo._2.map(tri =>
    Triangle(
      tri.poses.map(pos => (pos * scale))
    )
  )
  val (bottomCornerWorld, topCornerWorld): (Pos, Pos) = {
    val firstPos = poses.headOption.getOrElse(Pos(0, 0, 0))
    var minX = firstPos.x
    var minY = firstPos.y
    var minZ = firstPos.z

    var maxX = firstPos.x
    var maxY = firstPos.y
    var maxZ = firstPos.z
    poses.foreach(pos => {
      minX = Math.min(pos.x, minX)
      minY = Math.min(pos.y, minY)
      minZ = Math.min(pos.z, minZ)
      maxX = Math.max(pos.x, maxX)
      maxY = Math.max(pos.y, maxY)
      maxZ = Math.max(pos.z, maxZ)
    })
    (
      Pos(minX, minY, minZ),
      Pos(maxX, maxY, maxZ)
    )
  }
  val texture: BufferedImage = null
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
  ).map(pos =>
    pos
      .rotate(rotation)
      .translate(position)
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
  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)
  val texture: BufferedImage = null
}

object Cube extends Shapes {
  val position: Pos = Pos(0, 0, 0)
  val rotation: Pos = Pos(0, 0, 0)
  val poses = Vector[Pos](
    Pos(-100, -100, -100),
    Pos(100, -100, -100),
    Pos(100, 100, -100),
    Pos(100, 100, 100),
    Pos(-100, 100, 100),
    Pos(-100, -100, 100),
    Pos(100, -100, 100),
    Pos(-100, 100, -100)
  ).map(pos =>
    pos
      .rotate(rotation)
      .translate(position)
  )
  val triangles = Vector[Triangle](
    Triangle(
      (poses(0), poses(7), poses(2)),
      (Pos2D(0, 0), Pos2D(0, 159), Pos2D(159, 159))
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Pos2D(0, 0), Pos2D(159, 159), Pos2D(159, 0))
    ),
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

  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)
  val texture: BufferedImage = VisualizerApp.brickTexture
}
