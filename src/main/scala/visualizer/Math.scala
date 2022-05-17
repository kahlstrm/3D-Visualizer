package visualizer
import scala.math._
import java.awt.Color

object GfxMath {
  val width = VisualizerApp.width
  val height = VisualizerApp.height
  val fovinRadians = VisualizerApp.fov * math.Pi / 180.0
  val zNear = (width / 2.0) / tan(fovinRadians / 2.0)
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
  // point intersecting the zPlane and the line between pos1 and pos2
  def intersectPointWithZ(pos1: Pos, pos2: Pos): Pos = {
    val u = pos2 + (-pos1)
    val dot = zPlaneNormal.dotProduct(u)
    val w = pos1 + (-zPlane)
    val factor = -((zPlaneNormal.dotProduct(w)) / dot)
    val mul = (u * factor)
    return mul + pos1
  }

  def intersectPointWithPlane(
      pos1: Pos,
      pos2: Pos,
      plane: Pos,
      planeNormal: Pos
  ): Pos = {
    val planeNormalized = planeNormal.unit()
    val u = pos2 + (-pos1)
    val dot = planeNormalized.dotProduct(u)
    val w = pos1 + (-plane)
    val factor = -((planeNormalized.dotProduct(w)) / dot)
    val mul = (u * factor)
    return mul + pos1
  }
  def calcClipping(
      tri: Triangle,
      plane: Pos,
      planeNormal: Pos
  ): Vector[Triangle] = {
    var pointsOut = Vector[Pos]()
    var pointsIn = Vector[Pos]()

    tri.foreach(pos => {
      val unitPos = pos.unit()
      val planeNormalUnit = planeNormal.unit()
      val distanceFromPlane =
        (planeNormalUnit.x * pos.x + planeNormalUnit.y * pos.y + planeNormalUnit.z * pos.z - planeNormalUnit
          .dotProduct(plane));
      if (distanceFromPlane < 0) {
        pointsOut = pointsOut :+ pos
      } else pointsIn = pointsIn :+ pos
    })
    if (pointsIn.size == 3) {
      return Vector[Triangle](tri)
    }
    if (pointsIn.isEmpty) {
      return Vector[Triangle]()
    }
    if (pointsIn.size == 2) {
      val newpos1 =
        intersectPointWithPlane(pointsOut(0), pointsIn(0), plane, planeNormal)
      val newpos2 =
        intersectPointWithPlane(pointsOut(0), pointsIn(1), plane, planeNormal)
      return Vector[Triangle](
        Triangle(
          pointsIn(0),
          pointsIn(1),
          newpos1,
          tri.color
        ),
        Triangle(
          pointsIn(0),
          pointsIn(1),
          newpos2,
          tri.color
        )
      )
    }
    if (pointsIn.size == 1) {
      val newpos1 =
        intersectPointWithPlane(pointsOut(0), pointsIn(0), plane, planeNormal)
      val newpos2 =
        intersectPointWithPlane(pointsOut(1), pointsIn(0), plane, planeNormal)
      return Vector[Triangle](
        Triangle(
          pointsIn(0),
          newpos1,
          newpos2,
          tri.color
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
      this.x + GfxMath.width / 2,
      this.y + GfxMath.height / 2,
      this.z
    )
  }
  def perspective(): Pos = {
    Pos(
      this.x * GfxMath.zNear / z,
      this.y * GfxMath.zNear / z,
      this.z
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
    val z = 180 / Math.PI * cam.z
    f"x: $x%2.2f y: $y%2.2f z: $z%2.2f"
  }
}

object Pos {
  def apply(x: Double, y: Double, z: Double): Pos = {
    new Pos(x, y, z)
  }
  def apply(pos: Pos): Pos = {
    new Pos(pos.x, pos.y, pos.z)
  }
}

// A test for intersectPoint calculations
// object test extends App {
//   import GfxMath._
//   val a = Pos(0, 2, 0)
//   val b = Pos(5, 0, 3)
//   println(intersectPointWithPlane(a, b,Pos(0,5,1),Pos(1,2,6)))
//   println(intersectPointWithPlane(b, a,Pos(2,5,1),Pos(1,2,6)))
//   VisualizerApp.running=false
// }
