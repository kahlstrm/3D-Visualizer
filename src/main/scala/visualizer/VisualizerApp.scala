package visualizer
import scala.swing._
import scala.swing.event._
import java.awt.event._
import scala.concurrent._
import scala.concurrent.duration.Duration
import java.awt.Color
import Rendererer._
import misc._
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.WindowConstants
import java.awt.image.DataBufferDouble
object VisualizerApp extends App {
  implicit val ec: scala.concurrent.ExecutionContext =
    ExecutionContext.global
  System.setProperty("sun.java2d.opengl", "true");
  val (walls, playerPos) = FileLoader.loadFile("hello.map")
  val textures: Map[String, Texture] = Map(
    "stonebrick" ->
      Texture(FileLoader.loadTexture("stonebrick.png")),
    "dirt" -> Texture(FileLoader.loadTexture("minecraft.jpg"))
  )
  val worldObjects = walls ++ Vector[Shapes](
    // Object(
    //   FileLoader.loadObject("dragon.obj"),
    //   Pos(0, 0, 300),
    //   Pos(0, 0, 0),
    //   100
    // ),
    Cube(Pos(-100,0,0),Pos(0,0,0),"dirt"),
    Cube(Pos(100,0,0),Pos(0,0,0),"stonebrick")
  )

  val frame: JFrame = new JFrame("3d-visualizer")
  val futureFrameIterator = frameIterator
  var running = true
  var preRendering = false
  val renderDistance = 10000
  var triangleCount = 0
  var wireFrame = false
  var collisionEnabled = true
  var frametime = 0.0
  var othertime = 0.0
  var frames = 0
  val width = 1920
  val height = 1080
  val fov = 90
  var previousMouse: Option[Point] = None
  val windowHeight = height + 30
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
  val gc = area.getGraphicsConfiguration()
  val zBuffer= new DataBufferDouble(frame.getWidth*frame.getHeight())
  println(gc)
  def runGameNow() = {
    while (running) {
      update()
      //clear zBuffer
      for(i<- 0 until zBuffer.getSize()){
        zBuffer.setElemDouble(i,0.0)
      }
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
    g.fillRect(0, 0, frame.getWidth(), frame.getHeight())
    g.setColor(Color.WHITE)
    // if (preRendering) {
    //   drawFramesFuture(futureframeIterator.next(), g, wireFrame)
    // } else drawFrame(createFrames(Player.pos, Player.camera.pos), g, wireFrame)
    if (wireFrame) {
      drawFrame(createFrames(Player.pos, Player.camera.pos), g, wireFrame)
    } else
      g.drawImage(
        generateFrameImage(createFrames(Player.pos, Camera.pos),zBuffer),
        0,
        0,
        frame.getWidth(),
        frame.getHeight(),
        null
      )
    g.setColor(Color.GRAY)
    g.fillRect(40, 40, 300, 150)
    g.setColor(Color.WHITE)
    g.drawString("WASD to move, ESCAPE to close", 50, 60)
    g.drawString(Player.pos.toString(), 50, 80)
    g.drawString(Player.camera.toString(), 50, 100)
    g.drawString(
      f"frametime: $frametime%.3f s, other $othertime%.3f s",
      50,
      120
    )
    g.drawString(f"frames: $frames, $triangleCount triangles", 50, 140)
    g.drawString(
      "press R to toggle wireframe, C to toggle collision",
      50,
      160
    )
    g.drawString(
      s"rendering mode: ${if (preRendering) "multi" else "single"}, toggle M",
      50,
      180
    )
    drawCrosshair(g)
    val time = timeBetween(start, timeNanos())
    VisualizerApp.frametime = time
    g.dispose()
    bs.show()
  }
  val gameThread = new Thread(new Runnable {
    def run(): Unit = {
      try {
        println("game is now running")
        VisualizerApp.runGameNow
      } catch {
        case e: InterruptedException =>
        case a: Exception            => throw a
      }
    }
  })
  gameThread.start()
}
