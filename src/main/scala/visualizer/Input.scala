package visualizer

import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import VisualizerApp._
import misc._
import scala.swing.event.KeyReleased
import java.awt.event.MouseMotionListener
import java.awt.event.MouseEvent
object Input {
  val keyListener = new KeyListener {
    def keyPressed(e: KeyEvent): Unit = {
      val keyPress = new KeyPressed(e)
      keyPress.key match {
        case Key.Escape =>
          println("bye"); running = false; scala.sys.exit()
        case Key.W       => Player.moveForward = true
        case Key.S       => Player.moveBackward = true
        case Key.A       => Player.moveLeft = true
        case Key.D       => Player.moveRight = true
        case Key.Space   => Player.moveUp = true
        case Key.Shift   => Player.moveDown = true
        case Key.Control => Player.speedUp = true
        case Key.R       => wireFrame = !wireFrame
        case Key.C       => collisionEnabled = !collisionEnabled
        case a           => println(a)
      }
    }
    def keyReleased(e: KeyEvent): Unit = {
      val keyRelease = new KeyReleased(e)
      keyRelease.key match {
        case Key.W       => Player.moveForward = false
        case Key.S       => Player.moveBackward = false
        case Key.A       => Player.moveLeft = false
        case Key.D       => Player.moveRight = false
        case Key.Space   => Player.moveUp = false
        case Key.Shift   => Player.moveDown = false
        case Key.Control => Player.speedUp = false
        case _           =>
      }
    }
    def keyTyped(e: KeyEvent): Unit = {}
  }
  val mouseListener = new MouseMotionListener {
    def mouseMoved(e: MouseEvent): Unit = {
      val point = e.getPoint()
      if (previousMouse.isDefined && area.isFocusOwner()) {
        val prev = previousMouse.get
        val newCameraX = {
          val newVal =
            (Camera.x + (prev.x - point.x).toDouble / 500) % (2 * math.Pi)
          if (newVal > Math.PI) {
            (newVal - Math.PI * 2).toFloat
          } else if (newVal < -Math.PI) {
            (newVal + Math.PI * 2).toFloat
          } else newVal.toFloat
        }
        val newCameraY = ((-Math.PI / 2.0 + 0.00001) max
          (Camera.y + (prev.y - point.y).toDouble / 500) % (2 * math.Pi) min
          (Math.PI / 2.0 - 0.00001)).toFloat
        Camera.update(Vec3d(newCameraX, newCameraY, Camera.z))
        val centerOfWindow = area.getLocationOnScreen()
        robot.mouseMove(
          centerOfWindow.x + width / 2,
          centerOfWindow.y + height / 2
        );
        previousMouse = None
      } else {
        previousMouse = Some(point)
      }
    }
    def mouseDragged(e: MouseEvent): Unit = mouseMoved(e)
  }
}
