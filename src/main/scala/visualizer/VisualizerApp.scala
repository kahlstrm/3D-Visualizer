package visualizer
import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer
import java.awt.Color
import java.awt.event.ActionListener

object VisualizerApp extends SimpleSwingApplication {
  val width = 1600
  val height = 1200
  val fov = 90
  var previousMouse:Option[Point] = None
  val Wall = new Wall(
    Vector[Pos](
      Pos(0, 0, 0),
      Pos(600, 0, 0),
      Pos(600, 400, 0),
      Pos(600, 400, 200),
      Pos(0, 400, 200),
      Pos(0, 0, 200),
      Pos(600, 0, 200),
      Pos(0, 400, 0)
    ),
    Pos(-200, 0, 600),
    Pos(0, math.Pi / 2, 0)
  )
  val Wall2 = new Wall(
    Vector[Pos](
      Pos(0, 0, 0),
      Pos(600, 0, 0),
      Pos(600, 400, 0),
      Pos(600, 400, 200),
      Pos(0, 400, 200),
      Pos(0, 0, 200),
      Pos(600, 0, 200),
      Pos(0, 400, 0)
    ),
    Pos(0, 0, 600),
    Pos(0, 0, 0)
  )
  val windowHeight = height + 30
  val renderDistance = 1000.0
  def top = new MainFrame {
    title = "3d-visualizer"
    minimumSize = new Dimension(width, windowHeight)
    resizable = false

    val area = new Panel {
      focusable = true
      override def paintComponent(g: Graphics2D) = {
        println(Player.pos)
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, width, height)
        g.setColor(Color.WHITE)
        Wall.draw(g)
        Wall2.draw(g)
      }
    }
    contents = area
    listenTo(area.mouse.clicks)
    listenTo(area.mouse.moves)
    listenTo(area.keys)
    reactions += {
      case e: MouseDragged=>{
        print(e.point)
      }
      case KeyPressed(_, key, _, _) => {
        key match {
          case Key.Escape => println("bye"); scala.sys.exit()
          case Key.W      => Player.moveForward = true
          case Key.S      => Player.moveBackward = true
          case Key.A      => Player.moveLeft = true
          case Key.D      => Player.moveRight = true
          case Key.Space  => Player.moveUp = true
          case Key.Shift  => Player.moveDown = true
          case a          => println(a)
        }
      }
      case KeyReleased(_, key, _, _) => {
        key match {
          case Key.W     => Player.moveForward = false
          case Key.S     => Player.moveBackward = false
          case Key.A     => Player.moveLeft = false
          case Key.D     => Player.moveRight = false
          case Key.Space => Player.moveUp = false
          case Key.Shift => Player.moveDown = false
          case _         =>
        }
      }
      case MousePressed(_, point, _, _, _) => {
        println(point)
      }
      case MouseMoved(_,point,_)=>{
        if(previousMouse.isDefined){
        val prev = previousMouse.get
        Player.camera.y+=(prev.x-point.x).toDouble/100
        Player.camera.x+=(prev.y-point.y).toDouble/100
        print(Player.camera)
        previousMouse=Some(point)
        }else
          {
            previousMouse=Some(point)
          }
      }
    }
    
    val listener = new ActionListener() {
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        Player.move()
        area.repaint()
      }
    }
    val timer = new javax.swing.Timer(6, listener)
    timer.start()
  }
}
