package visualizer

object Player {
val pos = new Pos(0,0,0)
  

override def toString(): String = s"Player is currently at ${pos}"
}
