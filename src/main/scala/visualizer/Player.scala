package visualizer

object Player {
  val pos = new Pos(0, 0, 0)
  val camera = new Pos(0,0,0)
  
  def move() {
    if (moveForward) {
      pos.z+=20
    }
    if (moveBackward) {
      pos.z-=20
    }
    if (moveLeft) {
      pos.x-=20
    }
    if (moveRight) {
      pos.x+=20
    }
    if(moveUp){
      pos.y-=20
    }
    if(moveDown){
      pos.y+=20
    }
  }
  var moveForward = false
  var moveBackward = false
  var moveLeft = false
  var moveRight = false
  var moveUp = false
  var moveDown = false


  override def toString(): String = s"Player is currently at ${pos}"
}
