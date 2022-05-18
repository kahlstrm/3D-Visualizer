package visualizer
import java.awt.TexturePaint
import java.awt.Color
import scala.collection.parallel.CollectionConverters._
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.awt.image.DataBuffer
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
  def worldSpaceTris(player: Pos, camera: Pos) = {
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
  val texture: Texture
}

class Triangle(
    val poses: Array[Pos],
    val texPoses: Array[Pos] = null,
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
  def sortbyYAscNoTexture: Triangle = {
    // y1<y2
    if (pos1.y < pos2.y) {
      // y1<y2 & y1<y3
      if (pos1.y < pos3.y) {
        // y1<y2 & y1<y3 & y3<y2
        if (pos3.y < pos2.y) {
          return Triangle(
            (pos1, pos3, pos2),
            color
          )
        }
        // y1<y2 & y1<y3 & y3>y2
        return this
      }
      // y1<y2 & y3<y1
      return Triangle(
        (pos3, pos1, pos2),
        color
      )
    } else // y2<y1
      {
        if (pos3.y < pos2.y) {
          // y3<y2<y1
          return Triangle(
            (pos3, pos2, pos1),
            color
          )
        }
        // y2<y3 & y2<1
        if (pos3.y < pos1.y) {
          // y2<y3 & y2<1 & y3<y1
          return Triangle(
            (pos2, pos3, pos1),
            color
          )
        }

      }
    Triangle(
      (pos2, pos1, pos3),
      color
    )
  }
  // flip the triangles so that the the points are ordered by descending Y-value
  def sortbyYAsc: Triangle = {
    // y1<y2
    if (pos1.y < pos2.y) {
      // y1<y2 & y1<y3
      if (pos1.y < pos3.y) {
        // y1<y2 & y1<y3 & y3<y2
        if (pos3.y < pos2.y) {
          return Triangle(
            (pos1, pos3, pos2),
            (texPos1, texPos3, texPos2),
            color
          )
        }
        // y1<y2 & y1<y3 & y3>y2
        return this
      }
      // y1<y2 & y3<y1
      return Triangle(
        (pos3, pos1, pos2),
        (texPos3, texPos1, texPos2),
        color
      )
    } else // y2<y1
      {
        if (pos3.y < pos2.y) {
          // y3<y2<y1
          return Triangle(
            (pos3, pos2, pos1),
            (texPos3, texPos2, texPos1),
            color
          )
        }
        // y2<y3 & y2<1
        if (pos3.y < pos1.y) {
          // y2<y3 & y2<1 & y3<y1
          return Triangle(
            (pos2, pos3, pos1),
            (texPos2, texPos3, texPos1),
            color
          )
        }

      }
    Triangle(
      (pos2, pos1, pos3),
      (texPos2, texPos1, texPos3),
      color
    )
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
  def apply(pos1: Pos, pos2: Pos, pos3: Pos, texPoses: Array[Pos]) = {
    new Triangle(Array(pos1, pos2, pos3), texPoses)
  }
  def apply(
      pos1: Pos,
      pos2: Pos,
      pos3: Pos,
      texPoses: Array[Pos],
      col: Color
  ) = {
    new Triangle(Array(pos1, pos2, pos3), texPoses, col)
  }
  def apply(poses: (Pos, Pos, Pos), col: Color) = {
    new Triangle(Array(poses._1, poses._2, poses._3), color = col)
  }
  def apply(poses: (Pos, Pos, Pos), texPoses: (Pos, Pos, Pos), col: Color) = {
    new Triangle(
      Array(poses._1, poses._2, poses._3),
      texPoses = Array(texPoses._1, texPoses._2, texPoses._3),
      col
    )
  }
  def apply(poses: (Pos, Pos, Pos), texPoses: (Pos, Pos, Pos)) = {
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
  def apply(positions: Array[Pos], texPositions: Array[Pos], col: Color) = {
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
      tri.poses.map(pos =>
        (pos * scale)
          .rotate(rotation)
          .translate(position)
      )
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
  val texture: Texture = null
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
  val texture: Texture = null
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
      (Pos(0, 0), Pos(0, 1.0), Pos(1.0, 1.0))
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Pos(0, 0), Pos(1.0, 1.0), Pos(1.0, 0))
    ),
    Triangle((poses(1), poses(2), poses(3)),
      (Pos(0, 0), Pos(0, 1.0), Pos(1.0, 1.0))),
    Triangle((poses(1), poses(3), poses(6)),
      (Pos(0, 0), Pos(1.0, 1.0), Pos(1.0, 0))),
    Triangle((poses(6), poses(3), poses(4)),
      (Pos(0, 0), Pos(0, 1.0), Pos(1.0, 1.0))),
    Triangle((poses(6), poses(4), poses(5)),
      (Pos(0, 0), Pos(1.0, 1.0), Pos(1.0, 0))),
    Triangle((poses(5), poses(4), poses(7)),
      (Pos(0, 0), Pos(0, 1.0), Pos(1.0, 1.0))),
    Triangle((poses(5), poses(7), poses(0)),
      (Pos(0, 0), Pos(1.0, 1.0), Pos(1.0, 0))),
    Triangle((poses(7), poses(4), poses(3)),
      (Pos(0, 0), Pos(0, 1.0), Pos(1.0, 1.0))),
    Triangle((poses(7), poses(3), poses(2)),
      (Pos(0, 0), Pos(1.0, 1.0), Pos(1.0, 0))),
    Triangle((poses(6), poses(5), poses(0)),
      (Pos(0, 0), Pos(0, 1.0), Pos(1.0, 1.0))),
    Triangle((poses(6), poses(0), poses(1)),
      (Pos(0, 0), Pos(1.0, 1.0), Pos(1.0, 0)))
  )

  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)
  val texture: Texture = null
}

// simple texture class that provides color information
class Texture(image: BufferedImage) {
  private val texturePixels = image.getData().getDataBuffer()

  // returns colors
  def getColorPixel(x: Int, y: Int): Int = {
    if (x >= width || x < 0 || y >= height || x < 0) {
      println(f"x:$x y:$y out of range")
      return 0
    }
    return texturePixels.getElem(x + y * width)
  }
  def getColor(x: Double, y: Double): Int = {
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
