package visualizer
import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Queue
import scala.collection.parallel.CollectionConverters._
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.Success
import scala.util.Failure
import java.awt.Color
import java.awt.event.ActionListener
// import java.awt.TexturePaint
import Rendererer._
import misc._
object VisualizerApp extends SimpleSwingApplication {
  implicit val ec: scala.concurrent.ExecutionContext =
    ExecutionContext.global
  var wireFrame = false

  val (walls, playerPos) = FileLoader.loadFile("test.map")
  val worldObjects = walls ++ Vector[Shapes](
    new Object(
      FileLoader.loadObject("dragon.obj"),
      Pos(0, 0, 0),
      Pos(0, 0, 0),
      100
    )
  )
  var collisionEnabled = true
  private var frameIterator = Rendererer.createFrameIterator
  var frametime = 0.0
  val frametimes = Queue[Double]()
  var frametimeMulti = 0.0
  var avgFrametime = 0.0
  val width = 1600
  val height = 900
  val fov = 90
  var previousMouse: Option[Point] = None
  val windowHeight = height + 30
  def top = new MainFrame {
    title = "3d-visualizer"
    minimumSize = new Dimension(width, windowHeight)
    resizable = false
    val area = new Panel {
      focusable = true
      override def paintComponent(g: Graphics2D) = {
        val start = timeMillis()
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, width, height)
        g.setColor(Color.WHITE)
        Await.ready(
          drawFrames(frameIterator.next(), g, wireFrame),
          Duration.Inf
        )
        g.setColor(Color.GRAY)
        g.fillRect(40, 30, 300, 130)
        g.setColor(Color.WHITE)
        g.drawString("WASD to move, ESCAPE to close", 50, 50)
        g.drawString(Player.pos.toString(), 50, 70)
        g.drawString(Player.camera.toString(), 50, 90)
        g.drawString(
          f"frametime: $frametime%.3f AVG: $avgFrametime%.3f s",50, 110)
        g.drawString(f"frametime MT: $frametimeMulti%.3f s", 50, 130)
        g.drawString(
          "press R to toggle wireframe, C to toggle collision",
          50,
          150
        )
        drawCrosshair(g)
        val time = timeBetween(start,timeMillis())
        VisualizerApp.frametime = time
        frametimes.enqueue(time)
        while (frametimes.size > 50) {
          frametimes.dequeue
        }
        avgFrametime = frametimes.sum / frametimes.length
      }
    }
    contents = area
    area.cursor_=(emptyCursor)
    listenTo(area.mouse.moves)
    listenTo(area.keys)

    reactions += {
      case KeyPressed(_, key, _, _) => {
        key match {
          case Key.Escape => println("bye"); scala.sys.exit()
          case Key.W      => Player.moveForward = true
          case Key.S      => Player.moveBackward = true
          case Key.A      => Player.moveLeft = true
          case Key.D      => Player.moveRight = true
          case Key.Space  => Player.moveUp = true
          case Key.Shift  => Player.moveDown = true
          case Key.R      => wireFrame = !wireFrame
          case Key.C      => collisionEnabled = !collisionEnabled
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
      case MouseMoved(_, point, _) => {
        if (previousMouse.isDefined && this.area.peer.isFocusOwner()) {
          val prev = previousMouse.get
          Player.camera.x = {
            val newVal =
              (Player.camera.x + (prev.x - point.x).toDouble / 500) % (2 * math.Pi)
            if (newVal > Math.PI) {
              newVal - Math.PI * 2
            } else if (newVal < -Math.PI) {
              newVal + Math.PI * 2
            } else newVal
          }
          Player.camera.y = -Math.PI / 2.0 max
            (Player.camera.y + (prev.y - point.y).toDouble / 500) % (2 * math.Pi) min
            Math.PI / 2.0
          val centerOfWindow = area.peer.getLocationOnScreen()
          robot.mouseMove(
            centerOfWindow.x + width / 2,
            centerOfWindow.y + height / 2
          );
          previousMouse = None
        } else {
          previousMouse = Some(point)
        }
      }
    }

    val listener = new ActionListener() {
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        val oldPlayerPos = Player.move()
        if (collisionEnabled) {
          Future {
            val isInsideWall =
              !worldObjects.forall(!_.isInside(Player.pos))
            if (isInsideWall) {
              println("siellä on ihminen sisällä!")
              Player.updatePos(oldPlayerPos)
            }
          }
        }
        area.repaint()
      }

    }

    val timer = new javax.swing.Timer(4, listener)
    timer.start()

  }
}
