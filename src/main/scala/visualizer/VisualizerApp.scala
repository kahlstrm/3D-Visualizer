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
      case KeyPressed(src,Key.Escape,_,_)=>println("bye");scala.sys.exit()
      case KeyPressed(src,key,_,_)=>println(key)
      case MousePressed(src,point,_,_,_)=>println("hello team")
    }
  }
}
