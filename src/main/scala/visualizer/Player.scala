package visualizer

object Player {
  def pos = hiddenPos+Pos(0,0,0)
  val camera = Camera
  private var hiddenPos=VisualizerApp.playerPos
  def updatePos(pos:Pos)={
    hiddenPos=pos
  }
  def move():Pos ={
    val oldPos=pos
    val newPos=pos
    if (moveForward) {
      newPos.z+=20
    }
    if (moveBackward) {
      newPos.z-=20
    }
    if (moveLeft) {
      newPos.x-=20
    }
    if (moveRight) {
      newPos.x+=20
    }
    if(moveUp){
      newPos.y-=20
    }
    if(moveDown){
      newPos.y+=20
    }
    hiddenPos=newPos
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
