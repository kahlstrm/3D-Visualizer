package visualizer

class Math {
  
}


class Pos(private var xHidden:Double,private var yHidden:Double,private var zHidden:Double) {
  

  def x=xHidden
  def y=yHidden
  def z=zHidden
  def addX(amount:Double){
    xHidden+=amount
  }
  def addY(amount:Double){
    yHidden+=amount
  }
  def addZ(amount:Double){
    zHidden+=amount
  }
  def set(newX:Double,newY:Double,newZ:Double){
    xHidden=newX
    yHidden=newY
    zHidden=newX
  }
  override def toString(): String = s"x: ${xHidden} y: ${yHidden} z: ${zHidden}"
}