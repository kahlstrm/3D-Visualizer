package visualizer

object Player {
  def pos = hiddenPos + Vec3d(0, 0, 0)
  private var hiddenPos = VisualizerApp.playerPos
  private var time = misc.timeNanos()
  def move(collision: Boolean): Unit = {
    val movementSpeed =
      if (speedUp) 4000 * misc.timeBetween(time, misc.timeNanos())
      else 1000 * misc.timeBetween(time, misc.timeNanos())
    time = misc.timeNanos()
    val oldPos = pos
    var newPos = pos
    val forwardMove = Camera.forwardVector.dropY().unit()
    val rightMove = Camera.rightVector
    val upMove = Vec3d(0, 1, 0)
    var moveVecXZ = Vec3d(0, 0, 0)
    var moveVecY = Vec3d(0, 0, 0)
    if (moveForward) {
      moveVecXZ += (forwardMove)
    }
    if (moveBackward) {
      moveVecXZ += (-forwardMove)
    }
    if (moveLeft) {
      moveVecXZ += (-rightMove)
    }
    if (moveRight) {
      moveVecXZ += (rightMove)
    }
    if (moveUp) {
      moveVecY += (-upMove)
    }
    if (moveDown) {
      moveVecY += (upMove)
    }
    newPos += moveVecXZ.unit() * movementSpeed + moveVecY * movementSpeed
    if (collision) {
      val isInsideWall =
        VisualizerApp.worldObjects.find(_.isInside(newPos))
      if (isInsideWall.isDefined) {
        println("siellä on ihminen sisällä!")
      } else hiddenPos = newPos
    } else hiddenPos = newPos
  }
  var moveForward = false
  var moveBackward = false
  var moveLeft = false
  var moveRight = false
  var moveUp = false
  var moveDown = false
  var speedUp = false
  override def toString(): String = s"Player is currently at ${pos}"
}
