package visualizer
import java.io._
import scala.collection.mutable.Buffer
object FileLoader {
  def loadFile(source: String): (Array[Wall],Pos) = {

    val fileReader = try {
      new FileReader(source)
    }catch{
      case e: FileNotFoundException=>{
        println("map File not found")
        return (Array[Wall](),Pos(0,0,0))
      }
    }
    var playerPos=Pos(0,0,0)
    val walls = Buffer[Wall]()
    val lineReader = new BufferedReader(fileReader)
    var line =lineReader.readLine()
    var lineCounter=0
    while(line!=null){
      println(line)
      line.zipWithIndex.foreach({case (c,i)=>{
        c match{
          case '_'=> walls+=new Wall(Pos(i*600,0,lineCounter*(-600)),Pos(0, 0, 0))
          case '|'=> walls+=new Wall(Pos(-300+i*600,0,200+lineCounter*(-600)),Pos(0, Math.PI/2, 0))
          case 'S'=>playerPos=Pos(i*600,0,lineCounter*(-600))
          case _=>
        }
      }})
      lineCounter+=1
      line=lineReader.readLine()
    }
    fileReader.close()
    return (walls.toArray,playerPos)
  }
}
