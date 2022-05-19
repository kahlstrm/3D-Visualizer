package visualizer
import java.io._
import scala.collection.mutable.Buffer
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.GraphicsEnvironment
import VisualizerApp._
object FileLoader {
  def loadFile(source: String): (Vector[Shapes], Pos) = {
    val fileReader =
      try {
        new FileReader(s"maps/${source}")
      } catch {
        case e: FileNotFoundException => {
          println("map File not found")
          return (Vector[Shapes](), Pos(0, 0, 0))
        }
      }
    var playerPos = Pos(0, 0, 0)
    val walls = Buffer[Wall]()
    val lineReader = new BufferedReader(fileReader)
    var line = ""
    var lineCounter = 0
    while ({ line = lineReader.readLine(); line != null }) {
      println(line)
      line.zipWithIndex.foreach({
        case (c, i) => {
          c match {
            case '_' =>
              walls += new Wall(
                Pos(i * 600, 0, -200 + lineCounter * (-600)),
                Pos(0, 0, 0)
              )
            case '-' =>
              walls += new Wall(
                Pos(i * 600, 0, 200 + lineCounter * (-600)),
                Pos(0, 0, 0)
              )
            case '|' =>
              walls += new Wall(
                Pos(i * 600, 0, lineCounter * (-600)),
                Pos(0, Math.PI / 2, 0)
              )
            case 'S' => playerPos = Pos(i * 600, 0, lineCounter * (-600))
            case _   =>
          }
        }
      })
      lineCounter += 1
    }
    fileReader.close()
    return (walls.toVector, playerPos)
  }
  def loadTexture(source: String): BufferedImage = {
    val img =
      try {
        ImageIO.read(new File(source))
      } catch {
        case e: FileNotFoundException => {
          println("image File not found")
          throw e
        }
      }
    val gc = GraphicsEnvironment
      .getLocalGraphicsEnvironment()
      .getDefaultScreenDevice()
      .getDefaultConfiguration()
    val optimizedImg = gc.createCompatibleImage(img.getWidth(), img.getHeight())
    val g2d = optimizedImg.createGraphics()
    g2d.drawImage(img, 0, 0, null)
    g2d.dispose()
    optimizedImg
  }
  def loadObject(source: String): (Vector[Pos], Vector[Triangle]) = {
    val start = System.currentTimeMillis()
    val fileReader =
      try {
        new FileReader(s"objects/${source}")
      } catch {
        case e: FileNotFoundException => {
          throw new Exception("Object File not found, check path")
        }
      }
    val poses = Buffer[Pos]()
    val tris = Buffer[Vector[Int]]()
    val lineReader = new BufferedReader(fileReader)
    var line = ""
    while ({ line = lineReader.readLine(); line != null }) {
      val first = line.take(2).trim()
      first match {
        case "v" => {
          val rest = line.drop(2).strip
          val vectorCoords = rest.split(" ").map(_.toDouble)
          val newPos = Pos(vectorCoords(0), vectorCoords(1), vectorCoords(2))
          poses += newPos
        }
        case "f" => {
          val indices = line
            .drop(2)
            .strip
            .split(" ")
          if (indices.length == 3) {
            val newTri = indices.map(_.toInt - 1).toVector
            tris += newTri
          } else println("unsupported format")
        }
        case _ =>
      }
    }
    fileReader.close()
    val triangles =
      tris.map(n => Triangle(poses(n(0)), poses(n(1)), poses(n(2)))).toVector
    val end = System.currentTimeMillis()
    println(
      s"${source} ${poses.length} vertices and ${triangles.length} Triangles"
    )
    println(s"this took ${(end - start) / 1000.0} seconds")
    (poses.toVector, triangles.toVector)
  }
}

//test for .obj loading
// object loadTest extends App{
//   val obj=FileLoader.loadObject("pallo.obj")
//   // obj._2.foreach(println)
//   // println(obj._2.length)
// }
