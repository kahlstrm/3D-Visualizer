package visualizer
import scala.math._
object GfxMath {
  private val width = VisualizerApp.width
  private val height = VisualizerApp.height
  private val fovinRadians = VisualizerApp.fov * math.Pi / 180.0
  private val renderDistance = VisualizerApp.renderDistance
  private val zNear = (width / 2.0) / tan(fovinRadians / 2.0)
  
  def translatePos(pos: Pos, transpos: Pos): Pos = {
    Pos(
      pos.x + transpos.x,
      pos.y + transpos.y,
      pos.z + transpos.z
    )
  }

  def rotate(original:Pos,rotation:Pos)={
    Pos(
    	original.x * (cos(rotation.z) * cos(rotation.y)) + 
				 original.y * (cos(rotation.z) * sin(rotation.y) * sin(rotation.x) - sin(rotation.z) * cos(rotation.x)) +
				 original.z * (cos(rotation.z) * sin(rotation.y) * cos(rotation.x) + sin(rotation.z) * sin(rotation.x)),
	original.x * (sin(rotation.z) * cos(rotation.y)) +
				 original.y * (sin(rotation.z) * sin(rotation.y) * sin(rotation.x) + cos(rotation.z) * cos(rotation.x)) +
				 original.z * (sin(rotation.z) * sin(rotation.y) * cos(rotation.x) - cos(rotation.z) * sin(rotation.x)),
	original.x * (- sin(rotation.y)) +
				 original.y * (cos(rotation.y) * sin(rotation.x)) +
				 original.z * (cos(rotation.y) * cos(rotation.x)),
    )
  }
  def perspective(pos:Pos):Pos={
    Pos(
      pos.x*zNear/(zNear+pos.z),
      pos.y*zNear/(zNear+pos.z),
      pos.z
    )
  }

  def center(pos:Pos):Pos={
    Pos(
      pos.x+width/2,
      pos.y+height/2,
      pos.z
    )
  }
  def getNormal(tri:Triangle):Pos={
    val a= Pos(
      tri.pos2.x-tri.pos1.x,
      tri.pos2.y-tri.pos1.y,
      tri.pos2.y-tri.pos1.z
    )
    val b = Pos(
      tri.pos3.x-tri.pos1.x,
      tri.pos3.y-tri.pos1.y,
      tri.pos3.y-tri.pos1.z
    )
    val normalX=a.y*b.z-a.z*b.y
    val normalY=a.z*b.x-a.x*b.z
    val normalZ=a.x*b.y-a.y*b.x
    Pos(normalX,normalY,normalZ)
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
  def unary_-():Pos={
    Pos(
      -this.x,
      -this.y,
      -this.z
    )
  }
  override def toString(): String = s"x: ${x} y: ${y} z: ${z}"
}

object Pos {
  def apply(x: Double, y: Double, z: Double): Pos = {
    new Pos(x, y, z)
  }
  def apply(pos: Pos): Pos = {
    new Pos(pos.x, pos.y, pos.z)
  }
}
