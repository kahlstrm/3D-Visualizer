package visualizer
import scala.math._

object GfxMath {
  val screenWidth = VisualizerApp.realWidth
  val screenHeight = VisualizerApp.realHeight
  val fovinRadians = VisualizerApp.fov * math.Pi / 180.0
  val zNear = ((screenWidth / 2.0) / tan(fovinRadians / 2.0)).toFloat
  val zPlane = Pos(0, 0, 1)
  val zPlaneNormal = Pos(0, 0, 1)


  def getColor(tri: Triangle): Int = {
    val normal = tri.getNormal().unit()
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
  ): (Pos, Float) = {
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
  def newTexPos(texPosOut: Pos, texPosIn: Pos, fac: Float): Pos = {
    Pos(
      (1 - fac) * (texPosOut.x - texPosIn.x) + texPosIn.x,
      (1 - fac) * (texPosOut.y - texPosIn.y) + texPosIn.y,
      (1 - fac) * (texPosOut.z - texPosIn.z) + texPosIn.z
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
      if (tri.texPoses != null) {
        newTexPoses = newTexPoses
          .updated(0, newTexPos(tri.texPos1, tri.texPos3, fac1))
          .updated(1, newTexPos(tri.texPos2, tri.texPos3, fac2))
      }
      return Vector[Triangle](
        Triangle(
          // newpos1,
          // newpos2,
          // tri.pos3,
          tri.poses.updated(0, newpos1).updated(1, newpos2),
          newTexPoses,
          tri.color,
          tri.texture
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
      if (tri.texPoses != null) {
        newTexPoses = newTexPoses
          .updated(0, newTexPos(tri.texPos1, tri.texPos2, fac1))
          .updated(2, newTexPos(tri.texPos3, tri.texPos2, fac3))
      }
      return Vector[Triangle](
        Triangle(
          // newpos1,
          // tri.pos2,
          // newpos3,
          tri.poses.updated(0, newpos1).updated(2, newpos3),
          newTexPoses,
          tri.color,
          tri.texture
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
      if (tri.texPoses != null) {
        newTexPoses = newTexPoses
          .updated(1, newTexPos(tri.texPos2, tri.texPos1, fac2))
          .updated(2, newTexPos(tri.texPos3, tri.texPos1, fac3))
      }
      return Vector[Triangle](
        Triangle(
          // tri.pos1,
          // newpos2,
          // newpos3,
          tri.poses.updated(1, newpos2).updated(2, newpos3),
          newTexPoses,
          tri.color,
          tri.texture
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
      if (tri.texPoses != null) {
        val newTexPos1 = newTexPos(tri.texPos1, tri.texPos2, fac1)
        val newTexPos2 = newTexPos(tri.texPos1, tri.texPos3, fac2)
        newTexPoses = newTexPoses.updated(0, newTexPos1)
        newTexPoses2 = newTexPoses2
          .updated(0, newTexPos2)
          .updated(1, newTexPos1)
      }
      return Vector[Triangle](
        Triangle(
          // newpos1,
          // tri.pos2,
          // tri.pos3,
          tri.poses.updated(0, newpos1),
          newTexPoses,
          tri.color,
          tri.texture
        ),
        Triangle(
          // newpos2,
          // newpos1,
          // tri.pos3,
          tri.poses.updated(0, newpos2).updated(1, newpos1),
          newTexPoses2,
          tri.color,
          tri.texture
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
      if (tri.texPoses != null) {
        val newTexPos1 = newTexPos(tri.texPos2, tri.texPos1, fac1)
        val newTexPos2 = newTexPos(tri.texPos2, tri.texPos3, fac2)
        newTexPoses = newTexPoses.updated(1, newTexPos2)
        newTexPoses2 = newTexPoses2
          .updated(1, newTexPos1)
          .updated(2, newTexPos2)
      }
      return Vector[Triangle](
        Triangle(
          // tri.pos1,
          // newpos2,
          // tri.pos3,
          tri.poses.updated(1, newpos2),
          newTexPoses,
          tri.color,
          tri.texture
        ),
        Triangle(
          // tri.pos1,
          // newpos1,
          // newpos2,
          tri.poses.updated(1, newpos1).updated(2, newpos2),
          newTexPoses2,
          tri.color,
          tri.texture
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
      if (tri.texPoses != null) {
        val newTexPos1 = newTexPos(tri.texPos3, tri.texPos1, fac1)
        val newTexPos2 = newTexPos(tri.texPos3, tri.texPos2, fac2)
        newTexPoses = newTexPoses.updated(2, newTexPos1)
        newTexPoses2 = newTexPoses2
          .updated(0, newTexPos1)
          .updated(2, newTexPos2)
      }
      return Vector[Triangle](
        Triangle(
          // tri.pos1,
          // tri.pos2,
          // newpos1,
          tri.poses.updated(2, newpos1),
          newTexPoses,
          tri.color,
          tri.texture
        ),
        Triangle(
          // newpos1,
          // tri.pos2,
          // newpos2,
          tri.poses.updated(0, newpos1).updated(2, newpos2),
          newTexPoses2,
          tri.color,
          tri.texture
        )
      )
    }
    Vector[Triangle](tri)
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
