package visualizer

object Player {
  val pos = new Pos(0, 0, 0)

  def move() {
    if (moveForward) {
      pos.addZ(1)
    }
    if (moveBackward) {
      pos.addZ(-1)
    }
    if (moveLeft) {
      pos.addX(-1)
    }
    if (moveRight) {
      pos.addX(1)
    }
  }
  var moveForward = false
  var moveBackward = false
  var moveLeft = false
  var moveRight = false
  override def toString(): String = s"Player is currently at ${pos}"
}
