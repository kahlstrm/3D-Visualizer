package visualizer
import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer
import java.awt.Color
import java.awt.event.ActionListener
import java.awt.Robot

import java.awt.Toolkit
import java.awt.image.BufferedImage
object VisualizerApp extends SimpleSwingApplication {
  private val robot = new Robot()
  private val cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
  private val emptyCursor = Toolkit
    .getDefaultToolkit()
    .createCustomCursor(cursorImg, new Point(0, 0), "empty cursor")
  private val fileLoad = FileLoader.loadFile("test.map")
  Player.pos.update(fileLoad._2)
  val width = 1200
  val height = 800
  val fov = 90
  var previousMouse: Option[Point] = None
  // val Wall = new Wall(
  //   Pos(-300, 0, 600),
  //   Pos(0, math.Pi / 2, 0)
  // )
  // val Wall2 = new Wall(
  //   Pos(0, 0, 1000),
  //   Pos(0, 0, 0)
  // )
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
        // Wall.draw(g)
        // Wall2.draw(g)
        fileLoad._1.foreach(n=>n.draw(g))
        g.setColor(Color.WHITE)
        g.drawString("press ESCAPE to close", 50, 50)
        g.drawString(Player.pos.toString(), 50, 70)
        g.drawString(Player.camera.toString(), 50, 90)
        g.drawLine(width / 2, height / 2 + 10, width / 2, height / 2 - 10)
        g.drawLine(width / 2 + 10, height / 2, width / 2 - 10, height / 2)
      }
    }
    contents = area
    area.cursor_=(emptyCursor)
    listenTo(area.mouse.clicks)
    listenTo(area.mouse.moves)
    listenTo(area.keys)

    robot.mouseMove(width / 2, height / 2);
    reactions += {
      case e: MouseDragged => {
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
      case MouseMoved(_, point, _) => {
        if (previousMouse.isDefined) {
          val prev = previousMouse.get
          Player.camera.y =
            (Player.camera.y + (prev.x - point.x).toDouble / 100) % (2 * math.Pi)
          Player.camera.x = Math.max(
            -Math.PI / 2.0,
            Math.min(
              Math.PI / 2.0,
              (Player.camera.x - (prev.y - point.y).toDouble / 100) % (2 * math.Pi)
            )
          )
          print(Player.camera)
          robot.mouseMove(width / 2, height / 2);
          previousMouse = None
        } else {
          previousMouse = Some(point)
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
