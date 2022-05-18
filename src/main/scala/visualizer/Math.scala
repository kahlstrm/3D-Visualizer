package visualizer
import scala.math._
import java.awt.Color

object GfxMath {
  val screenWidth = VisualizerApp.frame.getWidth()
  val screenHeight = VisualizerApp.frame.getHeight()
  val fovinRadians = VisualizerApp.fov * math.Pi / 180.0
  val zNear = (screenWidth / 2.0) / tan(fovinRadians / 2.0)
  val zPlane = Pos(0, 0, 1)
  val zPlaneNormal = Pos(0, 0, 1)

  // calculate surface Normal https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal
  def getNormal(tri: Triangle): Pos = {
    val a = Pos(
      tri.pos2.x - tri.pos1.x,
      tri.pos2.y - tri.pos1.y,
      tri.pos2.z - tri.pos1.z
    )
    val b = Pos(
      tri.pos3.x - tri.pos1.x,
      tri.pos3.y - tri.pos1.y,
      tri.pos3.z - tri.pos1.z
    )
    val normalX = a.y * b.z - a.z * b.y
    val normalY = a.z * b.x - a.x * b.z
    val normalZ = a.x * b.y - a.y * b.x
    Pos(normalX, normalY, normalZ)
  }
  def getColor(tri: Triangle): Int = {
    val normal = getNormal(tri).unit()
    val avgPos = (tri.pos1 + tri.pos2 + tri.pos3) / 3
    // val playerPos =
    // Pos(VisualizerApp.width / 2, VisualizerApp.height / 2, 0)
    // val r = (avgPos).distance(playerPos) // "flashlight"
    val cosBetweenTriandZ = normal.dotProduct(Pos(0, 0, -1))
    // val rSquaredAndConstant = (Math.pow(r, 2) / 10000 + 1)
    val distanceFromZPlane = (avgPos).z / 2000 + 1 // "ambient light"
    (((225 / distanceFromZPlane).toInt + 30) * Math
      .sqrt(cosBetweenTriandZ)).toInt
  }

  // point intersecting the a plane and the line between pos1 and pos2
  def intersectPointWithPlane(
      pos1: Pos,
      pos2: Pos,
      plane: Pos,
      planeNormal: Pos
  ): (Pos, Double) = {
    val planeNormalized = planeNormal.unit()
    val u = pos2 + (-pos1)
    val dot = planeNormalized.dotProduct(u)
    val w = pos1 + (-plane)
    val factor = -((planeNormalized.dotProduct(w)) / dot)
    val mul = (u * factor)
    return (mul + pos1, factor)
  }
  def distanceFromPlane(pos: Pos, plane: Pos, planeNormalUnit: Pos) = {
    (planeNormalUnit.x * pos.x + planeNormalUnit.y * pos.y + planeNormalUnit.z * pos.z - planeNormalUnit
      .dotProduct(plane));
  }
  def newTexPos(texPosOut: Pos, texPosIn: Pos, fac: Double): Pos = {
    Pos(
      fac * (texPosOut.x - texPosIn.x) + texPosIn.x,
      fac * (texPosOut.y - texPosIn.y) + texPosIn.y,
      1
    )
  }
  // heavy spaghetti code to clip triangles so only triangles on screen show
  def calcClipping(
      tri: Triangle,
      plane: Pos,
      planeNormal: Pos
  ): Vector[Triangle] = {
    val planeNormalUnit = planeNormal.unit()

    val dist1 = distanceFromPlane(tri.pos1, plane, planeNormalUnit)
    val dist2 = distanceFromPlane(tri.pos2, plane, planeNormalUnit)
    val dist3 = distanceFromPlane(tri.pos3, plane, planeNormalUnit)

    // all points are outside the plane, no need to draw anything
    if (dist1 < 0 && dist2 < 0 && dist3 < 0) {
      return Vector[Triangle]()
    }
    // all points are inside the plane, just return current triangle
    if (dist1 > 0 && dist2 > 0 && dist3 > 0) {
      return Vector[Triangle](tri)
    }

    // points 1 and two 2 are outside the plane, return 1 clipped triangle
    if (dist1 < 0 && dist2 < 0) {
      val (newpos1, fac1) =
        intersectPointWithPlane(tri.pos1, tri.pos3, plane, planeNormalUnit)
      val (newpos2, fac2) =
        intersectPointWithPlane(tri.pos2, tri.pos3, plane, planeNormalUnit)
      var newTexPoses = tri.texPoses
      if (newTexPoses != null) {
        newTexPoses =
          newTexPoses.updated(0, newTexPos(tri.texPos1, tri.texPos3, fac1))
        newTexPoses(1) = newTexPos(tri.texPos2, tri.texPos3, fac2)
      }
      return Vector[Triangle](
        Triangle(
          // newpos1,
          // newpos2,
          // tri.pos3,
          tri.poses.updated(0, newpos1).updated(1, newpos2),
          newTexPoses,
         // tri.color
         Color.BLUE
        )
      )
    }

    // points 1 and two 3 are outside the plane, return 1 clipped triangle
    if (dist1 < 0 && dist3 < 0) {
      val (newpos1, fac1) =
        intersectPointWithPlane(tri.pos1, tri.pos2, plane, planeNormalUnit)

      val (newpos3, fac3) =
        intersectPointWithPlane(tri.pos3, tri.pos2, plane, planeNormalUnit)
      var newTexPoses = tri.texPoses
      if (newTexPoses != null) {
        newTexPoses =
          newTexPoses.updated(0, newTexPos(tri.texPos1, tri.texPos2, fac1))
        newTexPoses(2) = newTexPos(tri.texPos3, tri.texPos2, fac3)
      }
      return Vector[Triangle](
        Triangle(
          // newpos1,
          // tri.pos2,
          // newpos3,
          tri.poses.updated(0, newpos1).updated(2, newpos3),
          newTexPoses,
          // tri.color
          Color.BLUE
        )
      )
    }

    // points 2 and two 3 are outside the plane, return 1 clipped triangle
    if (dist2 < 0 && dist3 < 0) {
      val (newpos2, fac2) =
        intersectPointWithPlane(tri.pos2, tri.pos1, plane, planeNormalUnit)

      val (newpos3, fac3) =
        intersectPointWithPlane(tri.pos3, tri.pos1, plane, planeNormalUnit)
      var newTexPoses = tri.texPoses
      if (newTexPoses != null) {
        newTexPoses =
          newTexPoses.updated(1, newTexPos(tri.texPos2, tri.texPos1, fac2))
        newTexPoses(2) = newTexPos(tri.texPos3, tri.texPos1, fac3)
      }
      return Vector[Triangle](
        Triangle(
          // tri.pos1,
          // newpos2,
          // newpos3,
          tri.poses.updated(1, newpos2).updated(2, newpos3),
          newTexPoses,
          // tri.color
          Color.BLUE
        )
      )
    }

    // points 1 is outside the plane, return 2 clipped triangles
    if (dist1 < 0) {
      val (newpos1, fac1) =
        intersectPointWithPlane(tri.pos1, tri.pos2, plane, planeNormalUnit)

      val (newpos2, fac2) =
        intersectPointWithPlane(tri.pos1, tri.pos3, plane, planeNormalUnit)
      var newTexPoses = tri.texPoses
      var newTexPoses2 = tri.texPoses
      if (newTexPoses != null) {
        val newTexPos1 = newTexPos(tri.texPos1, tri.texPos2, fac1)
        val newTexPos2 = newTexPos(tri.texPos1, tri.texPos3, fac2)
        newTexPoses = newTexPoses.updated(0, newTexPos1)
        newTexPoses2 = newTexPoses2.updated(0, newTexPos2)
        newTexPoses2(1) = newTexPos1
      }
      return Vector[Triangle](
        Triangle(
          // newpos1,
          // tri.pos2,
          // tri.pos3,
          tri.poses.updated(0, newpos1),
          newTexPoses,
          // tri.color
          Color.GREEN
        ),
        Triangle(
          // newpos2,
          // newpos1,
          // tri.pos3,
          tri.poses.updated(0, newpos2).updated(1, newpos1),
          newTexPoses2,
          // tri.color
          Color.RED
        )
      )
    }

    // points 2 is outside the plane, return 2 clipped triangles
    if (dist2 < 0) {
      val (newpos1, fac1) =
        intersectPointWithPlane(tri.pos2, tri.pos1, plane, planeNormalUnit)
      val (newpos2, fac2) =
        intersectPointWithPlane(tri.pos2, tri.pos3, plane, planeNormalUnit)
      var newTexPoses = tri.texPoses
      var newTexPoses2 = tri.texPoses
      if (newTexPoses != null) {
        val newTexPos1 = newTexPos(tri.texPos2, tri.texPos1, fac1)
        val newTexPos2 = newTexPos(tri.texPos2, tri.texPos3, fac2)
        newTexPoses = newTexPoses.updated(1, newTexPos2)
        newTexPoses2 = newTexPoses2.updated(1, newTexPos1)
        newTexPoses2(2) = newTexPos2
      }
      return Vector[Triangle](
        Triangle(
          // tri.pos1,
          // newpos2,
          // tri.pos3,
          tri.poses.updated(1, newpos2),
          newTexPoses,
          // tri.color
          Color.GREEN
        ),
        Triangle(
          // tri.pos1,
          // newpos1,
          // newpos2,
          tri.poses.updated(1, newpos1).updated(2, newpos2),
          newTexPoses2,
          // tri.color
          Color.RED
        )
      )
    }

    // points 3 is outside the plane, return 2 clipped triangles
    if (dist3 < 0) {
      val (newpos1, fac1) =
        intersectPointWithPlane(tri.pos3, tri.pos1, plane, planeNormalUnit)
      val (newpos2, fac2) =
        intersectPointWithPlane(tri.pos3, tri.pos2, plane, planeNormalUnit)
      var newTexPoses = tri.texPoses
      var newTexPoses2 = tri.texPoses
      if (newTexPoses != null) {
        val newTexPos1 = newTexPos(tri.texPos3, tri.texPos1, fac1)
        val newTexPos2 = newTexPos(tri.texPos3, tri.texPos2, fac2)
        newTexPoses = newTexPoses.updated(2, newTexPos1)
        newTexPoses2 = newTexPoses2.updated(0, newTexPos1)
        newTexPoses2(2) = newTexPos2
      }
      return Vector[Triangle](
        Triangle(
          // tri.pos1,
          // tri.pos2,
          // newpos1,
          tri.poses.updated(2, newpos1),
          newTexPoses,
          // tri.color
          Color.GREEN
        ),
        Triangle(
          // newpos1,
          // tri.pos2,
          // newpos2,
          tri.poses.updated(0, newpos1).updated(2, newpos2),
          newTexPoses2,
          // tri.color
          Color.PINK
        )
      )
    }
    Vector[Triangle](tri)
  }

}

class Pos(
    var x: Double,
    var y: Double,
    var z: Double
) {

  def distance(that: Pos) = {
    math.sqrt(
      math.pow(that.x - this.x, 2) + math.pow(that.y - this.y, 2) + math.pow(
        that.z - this.z,
        2
      )
    )
  }
  def length = this.distance(Pos(0, 0, 0))

  def update(that: Pos): Unit = {
    this.x = that.x
    this.y = that.y
    this.z = that.z
  }
  def unary_-(): Pos = {
    Pos(
      -this.x,
      -this.y,
      -this.z
    )
  }
  def *(mul: Double): Pos = {
    Pos(
      this.x * mul,
      this.y * mul,
      this.z * mul
    )
  }
  def /(div: Double): Pos = {
    Pos(
      this.x / div,
      this.y / div,
      this.z / div
    )
  }
  def dotProduct(that: Pos): Double = {
    this.x * that.x + this.y * that.y + this.z * that.z
  }
  def crossProduct(that: Pos): Pos = {
    Pos(
      this.y * that.z - this.z * that.y,
      this.z * that.x - this.x * that.z,
      this.x * that.y - this.y * that.x
    )
  }
  def cosBetween(that: Pos): Double = {
    this.dotProduct(that) / (this.length * that.length)
  }

  def +(pos: Pos) = translate(pos)

  def translate(transpos: Pos): Pos = {
    Pos(
      this.x + transpos.x,
      this.y + transpos.y,
      this.z + transpos.z
    )
  }
  def center(): Pos = {
    Pos(
      this.x + VisualizerApp.width / 2,
      this.y + VisualizerApp.height / 2,
      this.z
    )
  }
  def perspective(): Pos = {
    Pos(
      this.x * GfxMath.zNear / z,
      this.y * GfxMath.zNear / z,
      z
    )
  }
  // fpsRotation matrix, took way too long to got this working
  // rotates the view by a specified angle, this is applied twice
  // first for the x-axis, then for the y-axis
  // https://www.3dgep.com/understanding-the-view-matrix/
  def xyzAxes(pitch: Double, y: Double) = {
    val yaw = y + Math.PI // get yaw range between 0-360
    val cosP = cos(pitch)
    val sinP = sin(pitch)
    val cosY = cos(yaw)
    val sinY = sin(yaw)
    val xAxis = -Pos(cosY, 0, -sinY) // fix up angles
    val yAxis = Pos(sinY * sinP, cosP, cosY * sinP)
    val zAxis = -Pos(sinY * cosP, -sinP, cosP * cosY)
    (xAxis, yAxis, zAxis)
  }
  def fpsRotate(pitch: Double, y: Double): Pos = {
    val (xAxis, yAxis, zAxis) = xyzAxes(pitch, y)
    Pos(
      this.x * xAxis.x + this.y * yAxis.x + this.z * zAxis.x,
      this.x * xAxis.y + this.y * yAxis.y + this.z * zAxis.y,
      this.x * xAxis.z + this.y * yAxis.z + this.z * zAxis.z
    )
  }
  // def cameraRotate(target:Pos,upVec:Pos):Pos = {
  //   val forwardVec= target.unit
  //   val right = upVec.unit.crossProduct(forwardVec)
  //   val up = forwardVec.crossProduct(right)

  //   Pos(
  //     this.x*right.x+this.y*up.x+this.z*forwardVec.x,
  //     this.x*right.y+this.y*up.y+this.z*forwardVec.y,
  //     this.x*right.z+this.y*up.z+this.z*forwardVec.z
  //   )
  // }
  // def cameraRotate():Pos ={
  //   this.rotate(Player.camera.cameraVector())
  // }
  def dropX(): Pos = {
    Pos(
      0,
      this.y,
      this.z
    )
  }
  def dropY(): Pos = {
    Pos(
      this.x,
      0,
      this.z
    )
  }
  def dropZ(): Pos = {
    Pos(
      this.x,
      this.y,
      0
    )
  }
  def unit(): Pos = if (this.length == 0) this else this / this.length
  def rotate(rotation: Pos): Pos = {
    Pos(
      this.x * (cos(rotation.z) * cos(rotation.y)) +
        this.y * (cos(rotation.z) * sin(rotation.y) * sin(rotation.x) - sin(
          rotation.z
        ) * cos(rotation.x)) +
        this.z * (cos(rotation.z) * sin(rotation.y) * cos(rotation.x) + sin(
          rotation.z
        ) * sin(rotation.x)),
      this.x * (sin(rotation.z) * cos(rotation.y)) +
        this.y * (sin(rotation.z) * sin(rotation.y) * sin(rotation.x) + cos(
          rotation.z
        ) * cos(rotation.x)) +
        this.z * (sin(rotation.z) * sin(rotation.y) * cos(rotation.x) - cos(
          rotation.z
        ) * sin(rotation.x)),
      this.x * (-sin(rotation.y)) +
        this.y * (cos(rotation.y) * sin(rotation.x)) +
        this.z * (cos(rotation.y) * cos(rotation.x))
    )
  }

  override def toString(): String = f"x: $x%2.2f y: $y%2.2f z: $z%2.2f"
}

object Camera extends Pos(0, 0, 0) {
  def pos() = this + Pos(0, 0, 0)
  // forwardVector for camera movement
  def forwardVector = {
    Pos(
      -cos(y) * sin(x),
      -sin(y),
      cos(y) * cos(x)
    ).unit()
  }
  // rightVector for camera movement
  def rightVector = {
    Pos(
      -sin(x - Math.PI / 2),
      0,
      cos(x - Math.PI / 2)
    ).unit()
  }
  // upvector, always orthogonal from forwad and right
  def testUp = {
    forwardVector.crossProduct(rightVector)
  }
  def cameraVector(): Pos = {
    Pos(
      this.y,
      this.x,
      0
    )
  }
  // def rightVector():Pos={
  //   // Pos(cos(y),0,sin(y)).unit()
  //   Pos(0,1,0).crossProduct(forwardVector())
  // }
  // def forwardVector():Pos= Pos(0,0,1).rotate(Player.camera.cameraVector()).unit
  // def upVector():Pos = {
  //   forwardVector().crossProduct(rightVector())
  //   // Pos(
  //   // sin(x)*sin(y),
  //   // cos(x),
  //   // -sin(x)*cos(y)
  //   // ).unit()
  // }
  override def toString(): String = {
    val cam = this.cameraVector()
    val x = 180 / Math.PI * cam.x
    val y = 180 / Math.PI * cam.y
    f"x: $x%2.2f y: $y%2.2f"
  }
}

object Pos {
  def apply(x: Double, y: Double, z: Double): Pos = {
    new Pos(x, y, z)
  }
  def apply(pos: Pos): Pos = {
    new Pos(pos.x, pos.y, pos.z)
  }
  def apply(x:Double,y:Double):Pos={
    new Pos(x,y,1)
  }
}

// A test for intersectPoint calculations

// object test extends App {
//   import GfxMath._
//   val a = Pos(4, -2, 3)
//   val b = Pos(-5, 6, 3)
//   println(intersectPointWithPlane(a, b,Pos(6,-5,1),Pos(646,2,-6)))
//   println(intersectPointWithPlane(b, a,Pos(6,-5,1),Pos(646,2,-6)))
//   VisualizerApp.running=false
// }
