package visualizer
import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer
import java.awt.Color
import java.awt.event.ActionListener

object VisualizerApp extends SimpleSwingApplication {
  val width = 800
  val height = 600
  val windowHeight = 630
  val aspectRatio = 800.toDouble / 600

  var triangles = Seq[Triangle]()
  val posBuffer = Buffer[Pos]()
  def top = new MainFrame {
    title = "3d-visualizer"
    minimumSize = new Dimension(width, windowHeight)
    resizable = false

    val area = new Panel {
      focusable = true
      override def paintComponent(g: Graphics2D) = {
        println(Player)
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, width, height)
        g.setColor(Color.WHITE)
        triangles.foreach(_.draw(g))
      }
    }
    contents = area
    listenTo(area.mouse.clicks)
    listenTo(area.keys)
    reactions += {
      case KeyPressed(_, key, _, _) => {
        key match {
          case Key.Escape => println("bye"); scala.sys.exit()
          case Key.W      => Player.moveForward = true
          case Key.S      => Player.moveBackward = true
          case Key.A      => Player.moveLeft = true
          case Key.D      => Player.moveRight = true
          case a          => println(a)
        }
      }
      case KeyReleased(_, key, _, _) => {
        key match {
          case Key.W => Player.moveForward = false
          case Key.S => Player.moveBackward = false
          case Key.A => Player.moveLeft = false
          case Key.D => Player.moveRight = false
        }
      }
      case MousePressed(_, point, _, _, _) => {
        println(point)
        posBuffer += new Pos(point.x, point.y, 0)
        if (posBuffer.size >= 3) {
          triangles =
            triangles :+ new Triangle(posBuffer(0), posBuffer(1), posBuffer(2))
          posBuffer.clear()
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
