package visualizer
import scala.swing._
import scala.swing.event._
object VisualizerApp extends SimpleSwingApplication{
  val width= 800
  val height= 600
  def top = new MainFrame{
    title="3d-visualizer"
    minimumSize = new Dimension(width,height)
    resizable=false
    
    val area = new Panel {
      focusable=true
      override def paintComponent(g: Graphics2D) = {
        g.setColor(new Color(0,0,0))
        g.fillRect(0,0, width,height)
        
      }
    }
    contents = area
    listenTo(area.mouse.clicks)
    listenTo(area.keys)
    reactions +={
      case KeyPressed(_,key,_,_)=>{
        key match {
          case Key.Escape=>println("bye");scala.sys.exit()
          case Key.W=>Player.pos.addZ(5)
          case Key.S=>Player.pos.addZ(-5)
          case Key.A=>Player.pos.addX(-5)
          case Key.D=>Player.pos.addX(5)
          case a=>println(a)
        }
      }
      case KeyReleased(_,key,_,_)=>{
        
      }
      case MousePressed(_,point,_,_,_)=>println(Player)
    }
  }
}
