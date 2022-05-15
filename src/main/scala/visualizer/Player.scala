package visualizer

object Player {
  def pos = hiddenPos + Pos(0, 0, 0)
  val camera = Camera
  private var hiddenPos = VisualizerApp.playerPos
  def updatePos(pos: Pos) = {
    hiddenPos = pos
  }
  private var time = misc.timeMillis()
  def move(): Pos = {
    val movementSpeed = 1000 * misc.timeBetween(time, misc.timeMillis())
    time = misc.timeMillis()
    val oldPos = pos
    var newPos = pos
    val forwardMove = Camera.forwardVector.unit()
    val rightMove = Camera.rightVector
    val upMove = Pos(0, 1, 0)
    var moveVecXZ = Pos(0, 0, 0)
    var moveVecY = Pos(0, 0, 0)
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
    hiddenPos = newPos
    oldPos
  }
  var moveForward = false
  var moveBackward = false
  var moveLeft = false
  var moveRight = false
  var moveUp = false
  var moveDown = false
  override def toString(): String = s"Player is currently at ${pos}"
}
