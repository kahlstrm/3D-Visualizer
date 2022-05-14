package visualizer
import scala.math._

object GfxMath {
  val width = VisualizerApp.width
  val height = VisualizerApp.height
  val fovinRadians = VisualizerApp.fov * math.Pi / 180.0
  val zNear = (width / 2.0) / tan(fovinRadians / 2.0)
  val zPlane=Pos(0,0,1)
  val zPlaneNormal=Pos(0,0,1)

  //calculate surface Normal https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal
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
  // point intersecting the zPlane and the line between pos1 and pos2
  def intersectPointWithZ(pos1: Pos, pos2: Pos): Pos = {
    val u = pos2 + (-pos1)
    val dot = zPlaneNormal.dotProduct(u)
    val w=pos1+(-zPlane)
    val factor = -((zPlaneNormal.dotProduct(w)) / dot)
    val mul = (u * factor)
    return  mul+pos1
  }
  def calcClipping(tri: Triangle): Vector[Triangle] = {
    if (tri.pos1.z < zPlane.z && tri.pos2.z < zPlane.z && tri.pos3.z < zPlane.z) {
      return Vector[Triangle]()
    }
    if (tri.pos1.z > zPlane.z && tri.pos2.z > zPlane.z && tri.pos3.z > zPlane.z) {
      return Vector[Triangle](tri)
    }
    if (tri.pos1.z < zPlane.z && tri.pos2.z < zPlane.z) {
      val newpos1 = intersectPointWithZ(tri.pos1, tri.pos3)
      val newpos2 = intersectPointWithZ(tri.pos2, tri.pos3)
      return Vector[Triangle](
        Triangle(
          newpos1,
          newpos2,
          tri.pos3
        )
      )
    }
    if (tri.pos1.z < zPlane.z && tri.pos3.z < zPlane.z) {
      val newpos1 = intersectPointWithZ(tri.pos1, tri.pos2)
      val newpos3 = intersectPointWithZ(tri.pos3, tri.pos2)
      return Vector[Triangle](
        Triangle(
          newpos1,
          tri.pos2,
          newpos3
        )
      )
    }
    if (tri.pos2.z < zPlane.z && tri.pos3.z < zPlane.z) {
      val newpos2 = intersectPointWithZ(tri.pos2, tri.pos1)
      val newpos3 = intersectPointWithZ(tri.pos3, tri.pos1)
      return Vector[Triangle](
        Triangle(
          tri.pos1,
          newpos2,
          newpos3
        )
      )
    }
    if (tri.pos1.z < zPlane.z) {
      val newpos1 = intersectPointWithZ(tri.pos1,tri.pos3)
      val newpos2 = intersectPointWithZ(tri.pos1,tri.pos2)
      return Vector[Triangle](
        Triangle(
          newpos1,
          tri.pos2,
          tri.pos3
        ),
        Triangle(
          newpos2,
          tri.pos2,
          tri.pos3
        )
      )
    }
    if (tri.pos2.z < zPlane.z) {
      val newpos1 = intersectPointWithZ(tri.pos2,tri.pos1)
      val newpos2 = intersectPointWithZ(tri.pos2,tri.pos3)
      return Vector[Triangle](
        Triangle(
          tri.pos1,
          newpos1,
          tri.pos3
        ),
        Triangle(
          tri.pos1,
          newpos2,
          tri.pos3
        )
      )
    }
    if (tri.pos3.z < zPlane.z) {
      val newpos1 = intersectPointWithZ(tri.pos3,tri.pos1)
      val newpos2 = intersectPointWithZ(tri.pos3,tri.pos2)
      return Vector[Triangle](
        Triangle(
          tri.pos1,
          tri.pos2,
          newpos1
        ),
        Triangle(
          tri.pos1,
          tri.pos2,
          newpos2
        )
      )
    }
    Vector[Triangle](tri)
  }
  // val proj = Array.ofDim[Double](4, 4)
  // proj(0)(0) = aspectRatio * fovMultiplier
  // proj(1)(1) = fovMultiplier
  // proj(2)(2) = zMultiplier
  // proj(3)(2) = -zNear * zMultiplier
  // proj(2)(3) = 1.0

  // println(proj(2)(2))
  // println(proj(3)(3))
  // def multiplyPos(pos: Pos, m: Array[Array[Double]]): Pos = {

  //   var newX = pos.x * m(0)(0) + pos.y * m(1)(0) + pos.z * m(2)(0) + m(3)(0)
  //   var newY = pos.x * m(0)(1) + pos.y * m(1)(1) + pos.z * m(2)(1) + m(3)(1)
  //   var newZ = pos.x * m(0)(2) + pos.y * m(1)(2) + pos.z * m(2)(2) + m(3)(2)
  //   val w = pos.x * m(0)(3) + pos.y * m(1)(3) + pos.z * m(2)(3) + m(3)(3)
  //   println(newY)
  //   println(w)
  //   if (w != 0) {
  //     newX /= w
  //     newY /= w
  //     newZ /= w
  //   }
  //   println(newY)
  //   Pos(newX, newZ, newY)
  // }
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
  def /(div:Double):Pos={
    Pos(
      this.x / div,
      this.y / div,
      this.z / div
    )
  }
  def dotProduct(that: Pos): Double = {
    this.x * that.x + this.y * that.y + this.z * that.z
  }
  def crossProduct(that:Pos):Pos={
    Pos(
    this.y * that.z - this.z * that.y,
    this.z * that.x - this.x * that.z,
    this.x * that.y - this.y * that.x)
  }
  def cosBetween(that:Pos):Double={
    this.dotProduct(that)/(this.length*that.length)
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
  //fpsRotation matrix, took way too long to got this working
  // rotates the view by a specified angle, this is applied twice
  // first for the x-axis, then for the y-axis
  // https://www.3dgep.com/understanding-the-view-matrix/
  def xyzAxes(pitch:Double,y:Double)={
    val yaw=y+Math.PI//get yaw range between 0-360
    val cosP=cos(pitch)
    val sinP=sin(pitch)
    val cosY=cos(yaw)
    val sinY=sin(yaw)
    val xAxis = -Pos(cosY,0,-sinY) //fix up angles
    val yAxis = Pos(sinY*sinP,cosP,cosY*sinP)
    val zAxis= -Pos(sinY*cosP,-sinP,cosP*cosY)
    (xAxis,yAxis,zAxis)
  }
  def fpsRotate(pitch:Double,y:Double):Pos={
    val (xAxis,yAxis,zAxis)=xyzAxes(pitch,y)
    Pos(
      this.x*xAxis.x+this.y*yAxis.x+this.z*zAxis.x ,
      this.x*xAxis.y+this.y*yAxis.y+this.z*zAxis.y,
      this.x*xAxis.z+this.y*yAxis.z+this.z*zAxis.z
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
  def dropX():Pos ={
    Pos(
      0,
      this.y,
      this.z
    )
  }
  def dropY():Pos ={
    Pos(
      this.x,
      0,
      this.z
    )
  }
  def dropZ():Pos ={
    Pos(
      this.x,
      this.y,
      0
    )
  }
  def unit():Pos=if (this.length==0) this else this/this.length
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
object Camera extends Pos(0,0,0) {

  //forwardVector for camera movement
  def forwardVector = {
    Pos(
      -cos(y)*sin(x),
      -sin(y),
      cos(y)*cos(x)
    ).unit()
  }
  //rightVector for camera movement
  def rightVector={
    Pos(
      -sin(x-Math.PI/2),
      0,
      cos(x-Math.PI/2)
    ).unit()
  }
  //upvector, always orthogonal from forwad and right
  def testUp={
    forwardVector.crossProduct(rightVector)
  }
  def cameraVector():Pos={
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
  override def toString(): String ={
    val cam = this.cameraVector()
    val x=180 / Math.PI * cam.x
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
object test extends App {
  import GfxMath._
  val a =Pos(0,2,0)
  val b = Pos(0,0,3)
  println(intersectPointWithZ(a,b))
  println(intersectPointWithZ(b,a))
}