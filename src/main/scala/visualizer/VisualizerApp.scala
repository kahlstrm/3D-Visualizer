package visualizer
import scala.swing._
import scala.concurrent._
import java.awt.Color
import Rendererer._
import misc._
import scala.collection.parallel.CollectionConverters._
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.WindowConstants
import java.awt.image.DataBufferFloat
object VisualizerApp extends App {
  implicit val ec: scala.concurrent.ExecutionContext =
    ExecutionContext.global
//USER CONFIG START
  val textures: Map[String, Texture] = Map(
    "stonebrick"     -> Texture("stone_bricks.png"),
    "dirt"           -> Texture("dirt.png"),
    "brick"          -> Texture("bricks.png"),
    "dark_oak_plank" -> Texture("dark_oak_planks.png")
  )
  val wallTexture = "dark_oak_plank"
  val floorTexture = "stonebrick"
  val (walls, floors, playerPos) = FileLoader.loadFile("hello.map", wallTexture, floorTexture) // to test other objects, I suggest using test.map
  val worldObjects = walls ++ floors ++ Vector[Shapes](
    // Object("dragon.obj",(0, 0, 0), (0, 0, 0), 100),                                         // a .obj with 210729 Triangles, quite big
    // Object("dragon_low_poly.obj", (8200, -100, -1800), (0, 0, 0), 100),                        // 10x smaller triangle count dragon, still quite big 
    Object("REALpallo.obj", (8200, -100, -1800), (0, 0, 0), 100),                                     // a ball object with 5940 Triangles
    Cube((7500, 100, -1800), (0, 0, 0), "dirt"),
    Cube((7300, 100, -1800), (0, 0, 0), "brick"),
    Cube((7100, 100, -1800), (0, 0, 0), "stonebrick")
  )
  val renderDistance = 20000
  val width = 1280
  val height = 720
  val fov = 90
  
  
  // USER CONFIG END
  val worldTris = worldObjects.flatMap(_.triangles).par
  var frames = 0
  var wireFrame = false
  var collisionEnabled = true
  val frame: JFrame = new JFrame("3d-visualizer")
  var frametime = 0.0f
  var othertime = 0.0f
  var triangleCount = 0
  var running = true
  var previousMouse: Option[Point] = None
  frame.setResizable(false)
  val area: JPanel = new JPanel {
    setPreferredSize(new Dimension(width, height))
    setFocusable(true)
    setIgnoreRepaint(true)
    setCursor(emptyCursor)
    addKeyListener(Input.keyListener)
    addMouseMotionListener(Input.mouseListener)
  }
  frame.add(area)
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.createBufferStrategy(3)
  frame.setIgnoreRepaint(true)
  val bs = frame.getBufferStrategy()
  val gc = frame.getGraphicsConfiguration()
  val realWidth = frame.getWidth()
  val realHeight = frame.getHeight()
  private val image = gc
    .createCompatibleImage(realWidth, realHeight)
  private val imagePixels = image.getRaster().getDataBuffer()
  private val zBuffer = new DataBufferFloat(realWidth * realHeight)
  private def runGameNow() = {
    while (running) {
      update()
      // clear Buffers
      for (i <- 0 until zBuffer.getSize()) {
        zBuffer.setElemFloat(i, 0.0f)
        imagePixels.setElem(i, 0)
      }
      render()
      frames += 1;
    }
	frame.dispose()
  }
  private def update() = {
    Player.move(collisionEnabled)
  }
  private def render() = {
    val g = bs.getDrawGraphics()
    val start = timeNanos()
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, realWidth, realHeight)
    g.setColor(Color.WHITE)
    if (wireFrame) {
      drawFrame(createFrameTriangles(Player.pos, Camera.pos()), g)
    } else {
      g.drawImage(
        generateFrameImage(
          createFrameTriangles(Player.pos, Camera.pos()),
          zBuffer,
          image
        ),
        0,
        0,
        realWidth,
        realHeight,
        null
      )
    }

    g.setColor(Color.GRAY)
    g.fillRect(40, 40, 290, 130)
    g.setColor(Color.WHITE)
    g.drawString("WASD to move, ESC to close, CTRL for speed", 50, 60)
    g.drawString(Player.pos.toString(), 50, 80)
    g.drawString(Camera.toString(), 50, 100)
    g.drawString(
      f"frametime: $frametime%.3f s, drawtime $othertime%.3f s",
      50,
      120
    )
    g.drawString(f"frames: $frames, $triangleCount triangles", 50, 140)
    g.drawString(
      "press R to toggle wireframe, C to toggle collision",
      50,
      160
    )
    drawCrosshair(g)
    val time = timeBetween(start, timeNanos())
    g.dispose()
    bs.show()
    VisualizerApp.frametime = time
  }
  runGameNow
}
