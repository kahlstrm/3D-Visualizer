package visualizer
import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer
import java.awt.Color
import java.awt.event.ActionListener
import java.awt.Robot
import java.awt.TexturePaint
import java.awt.Toolkit
import java.awt.image.BufferedImage
import scala.concurrent.Promise
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
object VisualizerApp extends SimpleSwingApplication {
  implicit val ec: scala.concurrent.ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
  val textureImg = FileLoader.loadTexture("minecraft.jpg")
  val texture =
    new TexturePaint(textureImg, new Rectangle(new Dimension(100, 100)))
  var wireFrame = false
  private val robot = new Robot()
  private val cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
  private val emptyCursor = Toolkit
    .getDefaultToolkit()
    .createCustomCursor(cursorImg, new Point(0, 0), "empty cursor")
  val (walls, playerPos) = FileLoader.loadFile("hello.map")
  val worldObjects = walls ++ Vector[Shapes](
    new Object(
      FileLoader.loadObject("dragon_low_poly.obj"),
      Pos(0, 0, 0),
      Pos(0, 0, 0),
      100
    )
  )
  private var drawFrame = Vector[Triangle]()
  private var renderComplete = true
  var frametimeSingle = 0.0
  var frametimeMulti = 0.0
  var randTimer = 0.0
  val width = 1280
  val height = 800
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
        val start = System.currentTimeMillis()

        g.setColor(Color.BLACK)
        g.fillRect(0, 0, width, height)
        g.setColor(Color.WHITE)
        // Wall.draw(g)
        // Wall2.draw(g)

        renderer.draw(g, drawFrame)
        g.setColor(Color.GRAY)
        g.fillRect(40, 30, 300, 110)
        g.setColor(Color.WHITE)
        g.drawString("press ESCAPE to close", 50, 50)
        g.drawString(Player.pos.toString(), 50, 70)
        g.drawString(Player.camera.toString(), 50, 90)
        g.drawString(s"frametime ST: ${frametimeSingle} s", 50, 110)
        g.drawString(s"frametime MT: ${frametimeMulti} s", 50, 130)
        g.drawString("press R to toggle wireframe", 50, 150)
        g.drawString(s"${randTimer}", 50, 170)
        g.drawLine(width / 2, height / 2 + 10, width / 2, height / 2 - 10)
        g.drawLine(width / 2 + 10, height / 2, width / 2 - 10, height / 2)
        val end = System.currentTimeMillis()
        VisualizerApp.frametimeSingle = (end - start) / 1000.0
      }
    }
    contents = area
    area.cursor_=(emptyCursor)
    listenTo(area.mouse.clicks)
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
          robot.mouseMove(width / 2, height / 2);
          previousMouse = None
        } else {
          previousMouse = Some(point)
        }
      }
    }

    val listener = new ActionListener() {
      def actionPerformed(e: java.awt.event.ActionEvent) = {

        val oldPlayerPos = Player.move()
        walls.foreach(n => {
          if (n.asInstanceOf[Wall].isInside(Pos(0, 0, 0))) {
            println("siel on ihminen sisällä!")
            Player.updatePos(oldPlayerPos)
          }
        })
        val start = System.currentTimeMillis()
        createFrames()
        val end = System.currentTimeMillis()
        VisualizerApp.frametimeMulti = (end - start) / 1000.0
        area.repaint()
      }

    }
    val timer = new javax.swing.Timer(8, listener)
    timer.start()

  }

    def createFrames()(implicit
        ec: ExecutionContext
    ): /*Future[Vector[Triangle]]*/ Unit = {
        val repainter = Promise[Vector[Triangle]]
        val worldSpaceTriangles = Future(worldObjects.flatMap(_.worldSpaceTris))
        worldSpaceTriangles.onComplete {
          case Success(value) =>
            repainter.completeWith(
              Future(renderer.generateViewTriangles(value))
            )
          case Failure(exception) => throw exception
        }
        val repaintF = repainter.future
        val p = Promise[Vector[Triangle]]
        repaintF.onComplete({
          case Success(frame) => drawFrame=frame
          case Failure(exception) => throw exception
        })
      

    }
}
