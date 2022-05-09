package visualizer
import java.io._
import scala.collection.mutable.Buffer
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
object FileLoader {
  def loadFile(source: String): (Array[Shapes],Pos) = {
    val fileReader = try {
      new FileReader(source)
    }catch{
      case e: FileNotFoundException=>{
        println("map File not found")
        return (Array[Shapes](),Pos(0,0,0))
      }
    }
    var playerPos=Pos(0,0,0)
    val walls = Buffer[Wall]()
    val lineReader = new BufferedReader(fileReader)
    var line =""
    var lineCounter=0
    while({line=lineReader.readLine();line!=null}){
      println(line)
      line.zipWithIndex.foreach({case (c,i)=>{
        c match{
          case '_'=> walls+=new Wall(Pos(i*600,0,-200+lineCounter*(-600)),Pos(0, 0, 0))
          case '-'=> walls+=new Wall(Pos(i*600,0,200+lineCounter*(-600)),Pos(0, 0, 0))
          case '|'=> walls+=new Wall(Pos(i*600,0,lineCounter*(-600)),Pos(0, Math.PI/2, 0))
          case 'S'=>playerPos=Pos(i*600,0,lineCounter*(-600))
          case _=>
        }
      }})
      lineCounter+=1
    }
    fileReader.close()
    return (walls.toArray,playerPos)
  }
  def loadTexture(source:String):BufferedImage={
    val img = try {
      ImageIO.read(new File(source))
    }catch{
      case e: FileNotFoundException=>{
        println("image File not found")
        throw e
      }
  }
  img
}
def loadObject(source:String):(Vector[Pos],Vector[Triangle])={
      val fileReader = try {
      new FileReader(source)
    }catch{
      case e: FileNotFoundException=>{
        println("Object File not found")
        return (Vector[Pos](),Vector[Triangle]())
      }
    }
val poses = Buffer[Pos]()
val tris = Buffer[Array[Int]]()
val lineReader = new BufferedReader(fileReader)
var line = ""
while({line=lineReader.readLine();line!=null}){
  val first = line.take(2).strip
  first match{
    case "v"=> {
      val rest = line.drop(2).strip
      val vectorCoords=rest.split(" ").map(_.toDouble)
      val newPos=Pos(vectorCoords(0),vectorCoords(1),vectorCoords(2)).unit
      poses+=newPos
      println(newPos)
    }
    case "f"=>{
      val indices = line.drop(2)
      .strip
      .split(" ")
      if(indices.length==3){
        val newTri = indices.map(_.head.asDigit-1)
        tris+=newTri
        println("Triangle of following poses:")
      }else if(indices.length==4){
        val pointsIndices=indices.map(_.head.asDigit-1)
        tris+=Array[Int](pointsIndices(0),pointsIndices(1),pointsIndices(2))
        tris+=Array[Int](pointsIndices(0),pointsIndices(2),pointsIndices(3))
      }else println("unsupported format")
    }
    case _=>
  }
}
fileReader.close()

(poses.toVector,tris.map(n=>new Triangle(poses(n(0)),poses(n(1)),poses(n(2)))).toVector)
}
}

object loadTest extends App{
  val obj=FileLoader.loadObject("cube.obj")
  obj._2.foreach(println)
}