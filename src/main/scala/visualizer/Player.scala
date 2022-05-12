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
    var newPos=pos
    val forwardMove= camera.forwardVector().dropY().unit()
    val rightMove=camera.rightVector().dropY().unit()
    val upMove=Pos(0,1,0)
    if (moveForward) {
      newPos=newPos.+(forwardMove*20)
    }
    if (moveBackward) {
      newPos=newPos.+(forwardMove*(-20))
    }
    if (moveLeft) {
      newPos=newPos.+(rightMove*(-20))
    }
    if (moveRight) {
      newPos=newPos.+(rightMove*20)
    }
    if(moveUp){
      newPos=newPos.+(upMove*(-20))
    }
    if(moveDown){
      newPos=newPos.+(upMove*20)
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
