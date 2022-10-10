package visualizer

import java.awt.Color
import java.awt.Graphics
class Triangle(
    val poses: Array[Vec3d],
    val texPoses: Array[Vec3d] = null,
    var color: Color = null,
    val texture: Texture = null
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

  // calculate surface Normal https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal
  def getNormal(): Vec3d = {
    val a = Vec3d(
      this.pos2.x - this.pos1.x,
      this.pos2.y - this.pos1.y,
      this.pos2.z - this.pos1.z
    )
    val b = Vec3d(
      this.pos3.x - this.pos1.x,
      this.pos3.y - this.pos1.y,
      this.pos3.z - this.pos1.z
    )
    val normalX = a.y * b.z - a.z * b.y
    val normalY = a.z * b.x - a.x * b.z
    val normalZ = a.x * b.y - a.y * b.x
    Vec3d(normalX, normalY, normalZ)
  }
  def worldSpace(player:Vec3d,camera:Vec3d): Triangle = {
    Triangle(
      this.poses.map(n =>
        n
          .translate(-player)
          .fpsRotate(0, camera.x)
          .fpsRotate(camera.y, 0)
      // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0)),
      ),
      this.texPoses,
      this.color,
      this.texture
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
          return new Triangle(
            Array(pos1, pos3, pos2),
            Array(texPos1, texPos3, texPos2),
            color,texture
          )
        }
        // y1<y2 & y1<y3 & y3>y2
        return this
      }
      // y1<y2 & y3<y1
      return new Triangle(
        Array(pos3, pos1, pos2),
        Array(texPos3, texPos1, texPos2),
        color,texture
      )
    } else // y2<y1
      {
        if (pos3.y < pos2.y) {
          // y3<y2<y1
          return new Triangle(
            Array(pos3, pos2, pos1),
            Array(texPos3, texPos2, texPos1),
            color,texture
          )
        }
        // y2<y3 & y2<1
        if (pos3.y < pos1.y) {
          // y2<y3 & y2<1 & y3<y1
          return new Triangle(
            Array(pos2, pos3, pos1),
            Array(texPos2, texPos3, texPos1),
            color,texture
          )
        }

      }
    new Triangle(
      Array(pos2, pos1, pos3),
      Array(texPos2, texPos1, texPos3),
      color,texture
    )
  }
  override def toString(): String =
    pos1.toString + pos2.toString + pos3.toString()
}

object Triangle {
  def apply(
      poses: (Vec3d, Vec3d, Vec3d),
      texPoses: (Vec3d, Vec3d, Vec3d),
      texture: Texture
  ) = {
    new Triangle(
      Array(poses._1, poses._2, poses._3),
      Array(texPoses._1, texPoses._2, texPoses._3),
      texture = texture
    )
  }
  def apply(poses: (Vec3d, Vec3d, Vec3d), texPoses: (Vec3d, Vec3d, Vec3d)) = {
    new Triangle(
      Array(poses._1, poses._2, poses._3),
      texPoses = Array(texPoses._1, texPoses._2, texPoses._3)
    )
  }
  def apply(pos1: Vec3d, pos2: Vec3d, pos3: Vec3d) = {
    new Triangle(
      Array(pos1, pos2, pos3),
      Array(Vec3d(1, 1), Vec3d(1, 0), Vec3d(0, 0))
    )
  }
  def apply(
      positions: Array[Vec3d],
      texPositions: Array[Vec3d],
      col: Color,
      texture: Texture
  ) = {
    new Triangle(
      positions,
      texPositions,
      col,
      texture
    )
  }
  def apply(
      positions: Array[Vec3d],
      texPositions: Array[Vec3d],
      texture: Texture
  ) = {
    new Triangle(
      positions,
      texPositions,
      texture = texture
    )
  }
}


// obsolete methods
  // def draw(g: Graphics, color: Color) = {
  //   g.setColor(color)
  //   g.fillPolygon(
  //     Array[Int](pos1.x.toInt, pos2.x.toInt, pos3.x.toInt),
  //     Array[Int](pos1.y.toInt, pos2.y.toInt, pos3.y.toInt),
  //     3
  //   )
  // }
  // def draw(g: Graphics, color: Int) = {
  //   g.setColor(new Color(color, color, color))
  //   g.fillPolygon(
  //     poses.map(_.x.toInt),
  //     poses.map(_.y.toInt),
  //     3
  //   )
  // }