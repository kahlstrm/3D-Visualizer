package visualizer
import Math._
class Vec3d(
    xCoord: Float,
    yCoord: Float,
    zCoord: Float
) {
  private var hiddenX = xCoord
  private var hiddenY = yCoord
  private var hiddenZ = zCoord
  def x = hiddenX
  def y = hiddenY
  def z = hiddenZ
  def distance(other: Vec3d) = {
    math.sqrt(
      math.pow(other.x - this.x, 2) + math.pow(other.y - this.y, 2) + math.pow(
        other.z - this.z,
        2
      )
    )
  }
  def length = this.distance(Vec3d(0, 0, 0))

  def update(other: Vec3d): Unit = {
    hiddenX = other.x
    hiddenY = other.y
    hiddenZ = other.z
  }
  def unary_- : Vec3d = {
    Vec3d(
      -this.x,
      -this.y,
      -this.z
    )
  }
  def *(mul: Float): Vec3d = {
    Vec3d(
      this.x * mul,
      this.y * mul,
      this.z * mul
    )
  }
  def /(div: Float): Vec3d = {
    Vec3d(
      this.x / div,
      this.y / div,
      this.z / div
    )
  }
  def dotProduct(other: Vec3d): Float = {
    this.x * other.x + this.y * other.y + this.z * other.z
  }
  def crossProduct(other: Vec3d): Vec3d = {
    Vec3d(
      this.y * other.z - this.z * other.y,
      this.z * other.x - this.x * other.z,
      this.x * other.y - this.y * other.x
    )
  }
  def cosBetween(other: Vec3d): Double = {
    this.dotProduct(other) / (this.length * other.length)
  }

  def +(pos: Vec3d) = translate(pos)

  def translate(transpos: Vec3d): Vec3d = {
    Vec3d(
      this.x + transpos.x,
      this.y + transpos.y,
      this.z + transpos.z
    )
  }
  def center(): Vec3d = {
    Vec3d(
      this.x + VisualizerApp.realWidth / 2,
      this.y + VisualizerApp.realHeight / 2,
      this.z
    )
  }
  def perspective(): Vec3d = {
    Vec3d(
      this.x * GfxMath.zNear / z,
      this.y * GfxMath.zNear / z,
      z
    )
  }
  def perspectiveTexture(posz: Float): Vec3d = {
    Vec3d(
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
    val xAxis = Vec3d(-cosY, 0, sinY) // fix up angles
    val yAxis = Vec3d(sinY * sinP, cosP, cosY * sinP)
    val zAxis = Vec3d(-sinY * cosP, sinP, -cosP * cosY)
    (xAxis, yAxis, zAxis)
  }
  def fpsRotate(pitch: Float, y: Float): Vec3d = {
    val (xAxis, yAxis, zAxis) = xyzAxes(pitch, y)
    Vec3d(
      this.x * xAxis.x + this.y * yAxis.x + this.z * zAxis.x,
      this.x * xAxis.y + this.y * yAxis.y + this.z * zAxis.y,
      this.x * xAxis.z + this.y * yAxis.z + this.z * zAxis.z
    )
  }
  // def cameraRotate(target:Vec3d,upVec:Vec3d):Vec3d = {
  //   val forwardVec= target.unit
  //   val right = upVec.unit.crossProduct(forwardVec)
  //   val up = forwardVec.crossProduct(right)

  //   Vec3d(
  //     this.x*right.x+this.y*up.x+this.z*forwardVec.x,
  //     this.x*right.y+this.y*up.y+this.z*forwardVec.y,
  //     this.x*right.z+this.y*up.z+this.z*forwardVec.z
  //   )
  // }
  // def cameraRotate():Vec3d ={
  //   this.rotate(Player.camera.cameraVector())
  // }
  def dropX(): Vec3d = {
    Vec3d(
      0,
      this.y,
      this.z
    )
  }
  def dropY(): Vec3d = {
    Vec3d(
      this.x,
      0,
      this.z
    )
  }
  def dropZ(): Vec3d = {
    Vec3d(
      this.x,
      this.y,
      0
    )
  }
  def unit(): Vec3d = if (this.length == 0) this else this / this.length.toFloat
  def rotate(rotation: Vec3d): Vec3d = {
    Vec3d(
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

object Camera extends Vec3d(0, 0, 0) {
  def pos() = this + Vec3d(0, 0, 0)
  // forwardVector for camera movement

  def forwardVector = {
    Vec3d(
      -cos(y) * sin(x),
      -sin(y),
      cos(y) * cos(x)
    ).unit()
  }
  // rightVector for camera movement
  def rightVector = {
    Vec3d(
      cos(x),
      0,
      sin(x)
    ).unit()
  }
  // upvector, always orthogonal from forwad and right
  def upVector = {
    forwardVector.crossProduct(rightVector)
  }
  def cameraVector(): Vec3d = {
    Vec3d(
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

object Vec3d {
  def apply(x: Double, y: Double, z: Double): Vec3d = {
    new Vec3d(x.toFloat, y.toFloat, z.toFloat)
  }
  def apply(x: Float, y: Float, z: Float): Vec3d = {
    new Vec3d(x, y, z)
  }
  def apply(pos: Vec3d): Vec3d = {
    new Vec3d(pos.x, pos.y, pos.z)
  }
  def apply(pos: (Float, Float, Float)) = {
    new Vec3d(pos._1, pos._2, pos._3)
  }
  def apply(x: Float, y: Float): Vec3d = {
    new Vec3d(x, y, 1)
  }
}
