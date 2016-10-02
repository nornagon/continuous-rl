package game

import kit.{Mat33, Vec2}
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.{FocusEvent, KeyboardEvent, MouseEvent, html}
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import kit.CanvasHelpers._


class World {
  var player = Vec2(0, 0)
  val mobs = mutable.Seq(Vec2(100, 100), Vec2(100, 300))
}

@JSExport
object Main {
  val world = new World
  var ctx: dom.CanvasRenderingContext2D = _
  val keysDown = mutable.Set[Int]()
  var mousePos: Vec2 = Vec2(0, 0)
  def mouseWorldPos: Vec2 = viewMat.inverse * mousePos
  var mouseDown: Boolean = false
  var viewMat: Mat33 = _

  def update(): Unit = {
    var acted = false
    if (keysDown contains KeyCode.Right) {
      world.player += Vec2(1, 0)
      acted = true
    }
    if (keysDown contains KeyCode.Left) {
      world.player -= Vec2(1, 0)
      acted = true
    }
    if (keysDown contains KeyCode.Up) {
      world.player -= Vec2(0, 1)
      acted = true
    }
    if (keysDown contains KeyCode.Down) {
      world.player += Vec2(0, 1)
      acted = true
    }
    if (mouseDown) {
      if ((world.player -> mouseWorldPos).length >= 4) {
        world.player += (world.player -> mouseWorldPos).normed * (if (keysDown.contains(KeyCode.Shift)) 3 else 1)
      }
      acted = true
    }
    if (acted) {
      for ((mob, i) <- world.mobs.zipWithIndex) {
        val delta = (mob -> world.player).normed * 0.8
        world.mobs(i) += delta
      }
    }
  }

  def draw(): Unit = {
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    ctx.save {
      ctx.transform(viewMat.a, viewMat.b, viewMat.d, viewMat.e, viewMat.c, viewMat.f)
      ctx.fillStyle = "blue"
      ctx.beginPath()
      ctx.arc(world.player.x, world.player.y, 4, 0, 2 * Math.PI)
      ctx.fill()
      ctx.strokeStyle = "blue"
      ctx.beginPath()
      ctx.arc(world.player.x, world.player.y, 12, 0, 2 * Math.PI)
      ctx.stroke()
      if ((world.player -> mouseWorldPos).length >= 4) {
        val pointer = world.player + (world.player -> mouseWorldPos).normed * 12
        ctx.beginPath()
        ctx.arc(pointer.x, pointer.y, 2, 0, 2 * Math.PI)
        ctx.fill()
      }

      ctx.fillStyle = "orange"
      for (mob <- world.mobs) {
        ctx.beginPath()
        ctx.arc(mob.x, mob.y, 4, 0, 2 * Math.PI)
        ctx.fill()
      }
    }
  }

  def frame(t: Double): Unit = {
    update()
    draw()
  }

  def run(): Unit = {
    def loop(t: Double): Unit = {
      frame(t)
      dom.window.requestAnimationFrame(loop _)
    }
    dom.window.requestAnimationFrame(loop _)
  }

  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth
    element.height = dom.window.innerHeight
    element.style.display = "block"
    root.appendChild(element)
    ctx = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    viewMat = Mat33.translate(ctx.canvas.width / 2, ctx.canvas.height / 2)
    dom.window.addEventListener("keydown", (e: KeyboardEvent) => { keysDown += e.keyCode })
    dom.window.addEventListener("keyup", (e: KeyboardEvent) => { keysDown -= e.keyCode })
    dom.window.addEventListener("blur", (e: FocusEvent) => { keysDown.clear(); mouseDown = false })
    dom.window.addEventListener("mousemove", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY) })
    dom.window.addEventListener("mousedown", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY); mouseDown = true })
    dom.window.addEventListener("mouseup", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY); mouseDown = false })
    run()
  }
}
