package visualizer
import scala.collection.parallel.CollectionConverters._


trait Shapes {
  val position: Pos
  val rotation: Pos
  val poses: Vector[Pos]
  // def worldSpacePos(player: Pos) = {
  //   poses.par.map(pos =>
  //     pos
  //       .translate(-player)
  //   )
  // }
  def worldSpaceTris(player: Pos, camera: Pos) = {
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


class Object(
    objInfo: (Vector[Pos], Vector[Triangle]),
    val position: Pos,
    val rotation: Pos,
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
object Object {
  def apply(
      objInfo: (Vector[Pos], Vector[Triangle]),
      position: Pos,
      rotation: Pos,
      scale: Float
  ) = new Object(objInfo, position, rotation, scale)
}
class Wall(val position: Pos, val rotation: Pos, textureString: String = null)
    extends Shapes {
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
      (Pos(0, 0), Pos(0, 2.0f), Pos(3.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Pos(0, 0), Pos(3.0f, 2.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(1), poses(2), poses(3)),
      (Pos(0, 0), Pos(0, 2.0f), Pos(1.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(1), poses(3), poses(6)),
      (Pos(0, 0), Pos(1.0f, 2.0f), Pos(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(3), poses(4)),
      (Pos(0, 0), Pos(0, 2.0f), Pos(3.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(4), poses(5)),
      (Pos(0, 0), Pos(3.0f, 2.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(5), poses(4), poses(7)),
      (Pos(0, 0), Pos(0, 2.0f), Pos(1.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(5), poses(7), poses(0)),
      (Pos(0, 0), Pos(1.0f, 2.0f), Pos(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(7), poses(4), poses(3)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(7), poses(3), poses(2)),
      (Pos(0, 0), Pos(3.0f, 1.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(5), poses(0)),
      (Pos(0, 0), Pos(0, 2.0f), Pos(1.0f, 2.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(0), poses(1)),
      (Pos(0, 0), Pos(1.0f, 2.0f), Pos(1.0f, 0)),
      texture
    )
  )
  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)
}
class Floor(val position: Pos, val rotation: Pos, textureString: String = null)
    extends Shapes {
  val poses = Vector[Pos](
    Pos(-300, -100, -300),
    Pos(300, -100, -300),
    Pos(300, 100, -300),
    Pos(300, 100, 300),
    Pos(-300, 100, 300),
    Pos(-300, -100, 300),
    Pos(300, -100, 300),
    Pos(-300, 100, -300)
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
      (Pos(0, 0), Pos(0, 1.0f), Pos(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Pos(0, 0), Pos(3.0f, 1.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(1), poses(2), poses(3)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(1), poses(3), poses(6)),
      (Pos(0, 0), Pos(3.0f, 1.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(3), poses(4)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(4), poses(5)),
      (Pos(0, 0), Pos(3.0f, 1.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(5), poses(4), poses(7)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(3.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(5), poses(7), poses(0)),
      (Pos(0, 0), Pos(3.0f, 1.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(7), poses(4), poses(3)),
      (Pos(0, 0), Pos(0, 3.0f), Pos(3.0f, 3.0f)),
      texture
    ),
    Triangle(
      (poses(7), poses(3), poses(2)),
      (Pos(0, 0), Pos(3.0f, 3.0f), Pos(3.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(5), poses(0)),
      (Pos(0, 0), Pos(0, 3.0f), Pos(3.0f, 3.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(0), poses(1)),
      (Pos(0, 0), Pos(3.0f, 3.0f), Pos(3.0f, 0)),
      texture
    )
  )
  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)
}


class Cube(val position: Pos, val rotation: Pos, textureString: String)
    extends Shapes {

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
      (Pos(0, 0), Pos(0, 1.0f), Pos(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(0), poses(2), poses(1)),
      (Pos(0, 0), Pos(1.0f, 1.0f), Pos(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(1), poses(2), poses(3)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(1), poses(3), poses(6)),
      (Pos(0, 0), Pos(1.0f, 1.0f), Pos(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(3), poses(4)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(4), poses(5)),
      (Pos(0, 0), Pos(1.0f, 1.0f), Pos(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(5), poses(4), poses(7)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(5), poses(7), poses(0)),
      (Pos(0, 0), Pos(1.0f, 1.0f), Pos(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(7), poses(4), poses(3)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(7), poses(3), poses(2)),
      (Pos(0, 0), Pos(1.0f, 1.0f), Pos(1.0f, 0)),
      texture
    ),
    Triangle(
      (poses(6), poses(5), poses(0)),
      (Pos(0, 0), Pos(0, 1.0f), Pos(1.0f, 1.0f)),
      texture
    ),
    Triangle(
      (poses(6), poses(0), poses(1)),
      (Pos(0, 0), Pos(1.0f, 1.0f), Pos(1.0f, 0)),
      texture
    )
  )

  val bottomCornerWorld = poses(0)
  val topCornerWorld = poses(3)

  override def toString(): String =
    s"Cube at $position"
}

object Cube {
  def apply(position: Pos, rotation: Pos, textureString: String) =
    new Cube(position, rotation, textureString)
}

