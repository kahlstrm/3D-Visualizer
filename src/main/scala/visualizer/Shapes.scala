package visualizer
import scala.collection.parallel.CollectionConverters._

trait Shapes {
  val position: Vec3d
  val rotation: Vec3d
  val poses: Vector[Vec3d]
  // def worldSpacePos(player: Pos) = {
  //   poses.par.map(pos =>
  //     pos
  //       .translate(-player)
  //   )
  // }
  def worldSpaceTris(player: Vec3d, camera: Vec3d) = {
    triangles.par.map(tri => {
      Triangle(
        tri.poses.map(n =>
          n
            .translate(-player)
            .fpsRotate(0, camera.x)
            .fpsRotate(camera.y, 0)
        // .cameraRotate(Camera.forwardVector().dropX(),Pos(0,1,0)),
        ),
        tri.texPoses,
        tri.color,
        tri.texture
      )
    })
  }
  val triangles: Vector[Triangle]
  val bottomCornerWorld: Vec3d
  val topCornerWorld: Vec3d

  def isInside(pos: Vec3d) = {
    def isBetween(a: Double, b: Double, c: Double) =
      Math.min(a, b) < c + 10 && Math.max(a, b) > c - 10
    isBetween(bottomCornerWorld.x, topCornerWorld.x, pos.x) &&
    isBetween(bottomCornerWorld.y, topCornerWorld.y, pos.y) &&
    isBetween(bottomCornerWorld.z, topCornerWorld.z, pos.z)
  }
  val texture: Texture
}

class Object(
    objInfo: (Vector[Vec3d], Vector[Triangle]),
    val position: Vec3d,
    val rotation: Vec3d,
    scale: Float
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
      ),
      tri.texPoses,
      tri.texture
    )
  )
  val (bottomCornerWorld, topCornerWorld): (Vec3d, Vec3d) = {
    val firstPos = poses.headOption.getOrElse(Vec3d(0, 0, 0))
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
      Vec3d(minX, minY, minZ),
      Vec3d(maxX, maxY, maxZ)
    )
  }
  val texture: Texture = null
}
object Object {
  def apply(
      objFile: String,
      pos: (Float, Float, Float),
      rot: (Float, Float, Float),
      scale: Float
  ) = new Object(FileLoader.loadObject(objFile), Vec3d(pos), Vec3d(rot), scale)
}
class Wall(
    val position: Vec3d,
    val rotation: Vec3d,
    textureString: String = null
) extends Shapes {
  val poses = Vector[Vec3d](
    Vec3d(-300, -200, -100),
    Vec3d(300, -200, -100),
    Vec3d(300, 200, -100),
    Vec3d(300, 200, 100),
    Vec3d(-300, 200, 100),
    Vec3d(-300, -200, 100),
    Vec3d(300, -200, 100),
    Vec3d(-300, 200, -100)
  ).map(pos =>
    pos
      .rotate(rotation)
      .translate(position)
  )
  val texture =
    if (textureString != null)
      try {
        VisualizerApp.textures(textureString)
      } catch {
        case _: NoSuchElementException =>
          println(
            s"No texture loaded called $textureString, loading no texture for $this"
          ); null
      }
    else null
  val triangles = Vector[Triangle](
    Triangle(
      (poses(0), poses(7), poses(2)),
      (Vec3d(0, 0), Vec3d(0, 2.0f), Vec3d(3.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Vec3d(0, 0), Vec3d(3.0f, 2.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(1), poses(2), poses(3)),
      (Vec3d(0, 0), Vec3d(0, 2.0f), Vec3d(1.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(1), poses(3), poses(6)),
      (Vec3d(0, 0), Vec3d(1.0f, 2.0f), Vec3d(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(3), poses(4)),
      (Vec3d(0, 0), Vec3d(0, 2.0f), Vec3d(3.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(4), poses(5)),
      (Vec3d(0, 0), Vec3d(3.0f, 2.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(5), poses(4), poses(7)),
      (Vec3d(0, 0), Vec3d(0, 2.0f), Vec3d(1.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(5), poses(7), poses(0)),
      (Vec3d(0, 0), Vec3d(1.0f, 2.0f), Vec3d(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(7), poses(4), poses(3)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(7), poses(3), poses(2)),
      (Vec3d(0, 0), Vec3d(3.0f, 1.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(5), poses(0)),
      (Vec3d(0, 0), Vec3d(0, 2.0f), Vec3d(1.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(0), poses(1)),
      (Vec3d(0, 0), Vec3d(1.0f, 2.0f), Vec3d(1.0f, 0)),
      texture
    )
  )
  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3) 
}
object Wall {
  def apply(
      pos: (Float, Float, Float),
      rotation: (Float, Float, Float),
      textureString: String
  ) =
    new Wall(Vec3d(pos), Vec3d(rotation), textureString)
}
class Floor(
    val position: Vec3d,
    val rotation: Vec3d,
    textureString: String = null
) extends Shapes {
  val poses = Vector[Vec3d](
    Vec3d(-300, -100, -300),
    Vec3d(300, -100, -300),
    Vec3d(300, 100, -300),
    Vec3d(300, 100, 300),
    Vec3d(-300, 100, 300),
    Vec3d(-300, -100, 300),
    Vec3d(300, -100, 300),
    Vec3d(-300, 100, -300)
  ).map(pos =>
    pos
      .rotate(rotation)
      .translate(position)
  )
  val texture =
    if (textureString != null)
      try {
        VisualizerApp.textures(textureString)
      } catch {
        case _: NoSuchElementException =>
          println(
            s"No texture loaded called $textureString, loading no texture for $this"
          ); null
      }
    else null
  val triangles = Vector[Triangle](
    Triangle(
      (poses(0), poses(7), poses(2)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Vec3d(0, 0), Vec3d(3.0f, 1.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(1), poses(2), poses(3)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(1), poses(3), poses(6)),
      (Vec3d(0, 0), Vec3d(3.0f, 1.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(3), poses(4)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(4), poses(5)),
      (Vec3d(0, 0), Vec3d(3.0f, 1.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(5), poses(4), poses(7)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(5), poses(7), poses(0)),
      (Vec3d(0, 0), Vec3d(3.0f, 1.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(7), poses(4), poses(3)),
      (Vec3d(0, 0), Vec3d(0, 3.0f), Vec3d(3.0f, 3.0f)),
      texture
    ),
    Triangle(
      (poses(7), poses(3), poses(2)),
      (Vec3d(0, 0), Vec3d(3.0f, 3.0f), Vec3d(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(5), poses(0)),
      (Vec3d(0, 0), Vec3d(0, 3.0f), Vec3d(3.0f, 3.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(0), poses(1)),
      (Vec3d(0, 0), Vec3d(3.0f, 3.0f), Vec3d(3.0f, 0)),
      texture
    )
  )
  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)
}
object Floor {
  def apply(
      pos: (Float, Float, Float),
      rotation: (Float, Float, Float),
      textureString: String
  ) =
    new Floor(Vec3d(pos), Vec3d(rotation), textureString)
}

class Cube(val position: Vec3d, val rotation: Vec3d, textureString: String)
    extends Shapes {

  val poses = Vector[Vec3d](
    Vec3d(-100, -100, -100),
    Vec3d(100, -100, -100),
    Vec3d(100, 100, -100),
    Vec3d(100, 100, 100),
    Vec3d(-100, 100, 100),
    Vec3d(-100, -100, 100),
    Vec3d(100, -100, 100),
    Vec3d(-100, 100, -100)
  ).map(pos =>
    pos
      .rotate(rotation)
      .translate(position)
  )
  val texture =
    if (textureString != null)
      try {
        VisualizerApp.textures(textureString)
      } catch {
        case _: NoSuchElementException =>
          println(
            s"No texture loaded called $textureString, loading no texture for $this"
          ); null
      }
    else null
  val triangles = Vector[Triangle](
    Triangle(
      (poses(0), poses(7), poses(2)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Vec3d(0, 0), Vec3d(1.0f, 1.0f), Vec3d(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(1), poses(2), poses(3)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(1), poses(3), poses(6)),
      (Vec3d(0, 0), Vec3d(1.0f, 1.0f), Vec3d(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(3), poses(4)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(4), poses(5)),
      (Vec3d(0, 0), Vec3d(1.0f, 1.0f), Vec3d(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(5), poses(4), poses(7)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(5), poses(7), poses(0)),
      (Vec3d(0, 0), Vec3d(1.0f, 1.0f), Vec3d(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(7), poses(4), poses(3)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(7), poses(3), poses(2)),
      (Vec3d(0, 0), Vec3d(1.0f, 1.0f), Vec3d(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(5), poses(0)),
      (Vec3d(0, 0), Vec3d(0, 1.0f), Vec3d(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(0), poses(1)),
      (Vec3d(0, 0), Vec3d(1.0f, 1.0f), Vec3d(1.0f, 0)),
      texture
    )
  )

  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)

  override def toString(): String =
    s"Cube at $position"
}

object Cube {
  def apply(
      pos: (Float, Float, Float),
      rotation: (Float, Float, Float),
      textureString: String
  ) =
    new Cube(Vec3d(pos), Vec3d(rotation), textureString)
}
