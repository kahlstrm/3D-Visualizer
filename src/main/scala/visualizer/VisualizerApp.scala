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

  val (walls, playerPos) = FileLoader.loadFile("test.map")
  val worldObjects = walls ++ Vector[Shapes](
    new Object(
      FileLoader.loadObject("dragon.obj"),
      Pos(0, 0, 0),
      Pos(0, 0, 0),
      100
    )
  )
  var running = true
  var wireFrame = false
  var collisionEnabled = true
  var frametime = 0.0
  var frames = 0
  val width = 1280
  val height = 800
  val fov = 90
  var previousMouse: Option[Point] = None
  val windowHeight = height + 30
  val top = new MainFrame {
    title = "3d-visualizer"
    minimumSize = new Dimension(width, windowHeight)
    resizable = false
    val area = new Panel {
      focusable = true
      peer.setIgnoreRepaint(true)
      cursor_=(emptyCursor)

      listenTo(mouse.moves)
      listenTo(keys)

      reactions += {
        case KeyPressed(_, key, _, _) => {
          key match {
            case Key.Escape =>
              println("bye"); running = false; scala.sys.exit(0)
            case Key.W     => Player.moveForward = true
            case Key.S     => Player.moveBackward = true
            case Key.A     => Player.moveLeft = true
            case Key.D     => Player.moveRight = true
            case Key.Space => Player.moveUp = true
            case Key.Shift => Player.moveDown = true
            case Key.R     => wireFrame = !wireFrame
            case Key.C     => collisionEnabled = !collisionEnabled
            case a         => println(a)
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
          if (previousMouse.isDefined && this.peer.isFocusOwner()) {
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
            val centerOfWindow = peer.getLocationOnScreen()
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
    }
    contents = area

  }

  top.peer.createBufferStrategy(3)
  top.peer.setIgnoreRepaint(true)
  val bs = top.peer.getBufferStrategy()

  def runGameNow() = {
    while (running) {
      update()
      render()
      frames += 1;
    }
  }
  def update() = {
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
  }
  def render() = {
    val g = bs.getDrawGraphics()
    val start = timeNanos()
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, windowHeight)
    g.setColor(Color.WHITE)
    drawFrame(createFrames(Player.pos,Player.camera.pos),g,wireFrame)
    g.setColor(Color.GRAY)
    g.fillRect(40, 40, 300, 130)
    g.setColor(Color.WHITE)
    g.drawString("WASD to move, ESCAPE to close", 50, 60)
    g.drawString(Player.pos.toString(), 50, 80)
    g.drawString(Player.camera.toString(), 50, 100)
    g.drawString(
      f"frametime: $frametime%.3f s",
      50,
      120
    )
    g.drawString(f"frames: $frames", 50, 140)
    g.drawString(
      "press R to toggle wireframe, C to toggle collision",
      50,
      160
    )
    drawCrosshair(g)
    val time = timeBetween(start, timeNanos())
    VisualizerApp.frametime = time
    g.dispose()
    bs.show()
  }
  val gameThread = new Thread(Runnable)
  gameThread.start()
}
object Runnable extends Runnable {
  def run(): Unit = {
    try {
      VisualizerApp.runGameNow
    } catch {
      case e: InterruptedException =>
    }
  }
}
