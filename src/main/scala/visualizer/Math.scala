package visualizer
import scala.math._
object GfxMath {
  val width = VisualizerApp.width
  val height = VisualizerApp.height
  val fovinRadians = VisualizerApp.fov * math.Pi / 180.0
  val renderDistance = VisualizerApp.renderDistance
  val zNear = (width / 2.0) / tan(fovinRadians / 2.0)
  val zPlane=Pos(0,0,1)
  val zPlaneNormal=Pos(0,0,1)
  def getNormal(tri: Triangle): Pos = {
    val a = Pos(
      tri.pos2.x - tri.pos1.x,
      tri.pos2.y - tri.pos1.y,
      tri.pos2.y - tri.pos1.z
    )
    val b = Pos(
      tri.pos3.x - tri.pos1.x,
      tri.pos3.y - tri.pos1.y,
      tri.pos3.y - tri.pos1.z
    )
    val normalX = a.y * b.z - a.z * b.y
    val normalY = a.z * b.x - a.x * b.z
    val normalZ = a.x * b.y - a.y * b.x
    Pos(normalX, normalY, normalZ)
  }
  def intersectPointWithZ(pos1: Pos, pos2: Pos): Pos = {
    val u = pos2 + (-pos1)
    val dot = zPlaneNormal.dotProduct(u)
    val w=pos1+(-zPlane)
    val factor = -((zPlaneNormal.dotProduct(w)) / dot)
    val mul = (u * factor)
    return  mul+pos1
  }
  def calcClipping(tri: Triangle): Array[Triangle] = {
    if (tri.pos1.z < zPlane.z && tri.pos2.z < zPlane.z && tri.pos3.z < zPlane.z) {
      return Array[Triangle]()
    }
    if (tri.pos1.z < zPlane.z && tri.pos2.z < zPlane.z) {
      val newpos1 = intersectPointWithZ(tri.pos1, tri.pos3)
      val newpos2 = intersectPointWithZ(tri.pos2, tri.pos3)
      return Array[Triangle](
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
      return Array[Triangle](
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
      return Array[Triangle](
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
      return Array[Triangle](
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
      return Array[Triangle](
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
      return Array[Triangle](
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
    Array[Triangle](tri)
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
  def dotProduct(that: Pos): Double = {
    this.x * that.x + this.y * that.y + this.z * that.z
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

  override def toString(): String = s"x: ${x} y: ${y} z: ${z}"
}
object Camera extends Pos(0, 0, 0) {
  
  def rightVector():Pos={
    Pos(cos(y),0,sin(y))
  }
  def forwardVector():Pos={
    rightVector().rotate(Pos(0,-Math.PI/2,0))
  }
  override def toString(): String =
    s"x: ${180 / Math.PI * x} y: ${180 / Math.PI * y} z: ${180 / Math.PI * z}"
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