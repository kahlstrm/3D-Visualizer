package visualizer

import java.awt.Graphics2D

trait Shapes {
  def draw(g:Graphics2D):Unit
}

class Triangle(pos1:Pos,pos2:Pos,pos3:Pos) extends Shapes{

  def draw(g:Graphics2D)={
    g.drawLine(pos1.x.toInt,pos1.y.toInt,pos2.x.toInt,pos2.y.toInt)
    g.drawLine(pos2.x.toInt,pos2.y.toInt,pos3.x.toInt,pos3.y.toInt)
    g.drawLine(pos3.x.toInt,pos3.y.toInt,pos1.x.toInt,pos1.y.toInt)
  }


}