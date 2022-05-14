package visualizer

object Player {
  def pos = hiddenPos+Pos(0,0,0)
  val camera = Camera
  private var hiddenPos=VisualizerApp.playerPos
  def updatePos(pos:Pos)={
    hiddenPos=pos
  }
  def move():Pos ={
    val movementSpeed=20
    val oldPos=pos
    var newPos=pos
    val forwardMove=Camera.test
    val rightMove=camera.rightVector().dropY.unit()
    val upMove=camera.upVector().dropX().dropZ().unit()
    if (moveForward) {
      newPos=newPos.+(forwardMove*movementSpeed)
    }
    if (moveBackward) {
      newPos=newPos.+(forwardMove*(-movementSpeed))
    }
    if (moveLeft) {
      newPos=newPos.+(rightMove*(-movementSpeed))
    }
    if (moveRight) {
      newPos=newPos.+(rightMove*movementSpeed)
    }
    if(moveUp){
      newPos=newPos.+(upMove* (-movementSpeed))
    }
    if(moveDown){
      newPos=newPos.+(upMove* movementSpeed)
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
