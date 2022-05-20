package visualizer
import Math._
class Pos(
    var x: Float,
    var y: Float,
    var z: Float
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
  def unary_- : Pos = {
    Pos(
      -this.x,
      -this.y,
      -this.z
    )
  }
  def *(mul: Float): Pos = {
    Pos(
      this.x * mul,
      this.y * mul,
      this.z * mul
    )
  }
  def /(div: Float): Pos = {
    Pos(
      this.x / div,
      this.y / div,
      this.z / div
    )
  }
  def dotProduct(that: Pos): Float = {
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
      this.x + VisualizerApp.realWidth / 2,
      this.y + VisualizerApp.realHeight / 2,
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
  def perspectiveTexture(posz: Float): Pos = {
    Pos(
      this.x * GfxMath.zNear / posz,
      this.y * GfxMath.zNear / posz,
      (GfxMath.zNear / posz)
    )
  }
  // fpsRotation matrix, took way too long to got this working
  // rotates the view by a specified angle, this is applied twice
  // first for the x-axis, then for the y-axis
  // https://www.3dgep.com/understanding-the-view-matrix/
  def xyzAxes(pitch: Float, y: Float) = {
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
  def fpsRotate(pitch: Float, y: Float): Pos = {
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
  def unit(): Pos = if (this.length == 0) this else this / this.length.toFloat
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
  def upVector = {
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
    new Pos(x.toFloat, y.toFloat, z.toFloat)
  }
  def apply(x: Float, y: Float, z: Float): Pos = {
    new Pos(x, y, z)
  }
  def apply(pos: Pos): Pos = {
    new Pos(pos.x, pos.y, pos.z)
  }
  def apply(pos:(Float,Float,Float))={
    new Pos(pos._1,pos._2,pos._3)
  }
  def apply(x: Float, y: Float): Pos = {
    new Pos(x, y, 1)
  }
}