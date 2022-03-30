package visualizer

class Math {
  
}


class Pos(private var xHidden:Double,private var yHidden:Double,private var zHidden:Double) {
  

  def x=xHidden
  def y=yHidden
  def z=zHidden
  def addX(x:Double){
    xHidden+=x
  }
  def addY(y:Double){
    yHidden+=y
  }
  def addZ(y:Double){
    zHidden+=y
  }
  def add(x:Double,y:Double,z:Double){
    addX(x)
    addY(y)
    addZ(z)
  }
  def set(newX:Double,newY:Double,newZ:Double){
    xHidden=newX
    yHidden=newY
    zHidden=newX
  }
  override def toString(): String = s"x: ${xHidden} y: ${yHidden} z: ${zHidden}"
}