package game

import kit.{Circle2, Mat33, Segment2, Vec2}
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.{FocusEvent, KeyboardEvent, MouseEvent, html}
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import kit.CanvasHelpers._


class World {
  var player = Vec2(0, 0)
  var mobs = mutable.Seq(Vec2(100, 100), Vec2(100, 300))
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

  var attacking = false
  var attackDirection: Vec2 = Vec2(1, 0)
  var attackT = 0d

  def sword: Segment2 = {
    val d = attackT / attackLength * Math.PI
    val extension = Math.sin(d) * 10
    val base = world.player + (world.player -> mouseWorldPos).normed.perp * 3
    Segment2(base, base + Vec2.forAngle(attackDirection.toAngle + Math.cos(d)) * extension)
  }

  val attackLength = 25

  def update(): Unit = {
    var acted = false
    if ((keysDown contains KeyCode.Space) && !attacking && (world.player -> mouseWorldPos).length >= 4) {
      attacking = true
      attackDirection = (world.player -> mouseWorldPos).normed
      attackT = 0
      acted = true
    } else if (attacking) {
      attackT += 1
      world.mobs = world.mobs.filterNot { mob => sword intersects Circle2(mob, 4) }
      acted = true
      if (attackT >= attackLength) {
        attacking = false
      }
    } else {
      if (mouseDown) {
        if ((world.player -> mouseWorldPos).length >= 4) {
          world.player += (world.player -> mouseWorldPos).normed * (if (keysDown.contains(KeyCode.Shift)) 3 else 1)
        }
        acted = true
      }
    }
    if (acted) {
      if (Math.random() < 0.01) {
        world.mobs :+= Vec2(Math.random() * ctx.canvas.width - ctx.canvas.width / 2, Math.random() * ctx.canvas.height - ctx.canvas.height / 2)
      }
      for ((mob, i) <- world.mobs.zipWithIndex) {
        val nearby = world.mobs.zipWithIndex.filter(_._2 != i).map(_._1)
        def f(separation: Double) = Math.log(separation / 40) / separation
        val nbForce = nearby.map(mob -> _).map(v => v.normed * f(v.length)).reduceOption(_ + _).getOrElse(Vec2(0,0))
        val playerForce = (mob -> world.player).normed * 1
        //val delta = (mob -> world.player).normed * 0.8
        val delta = (nbForce + playerForce).normed * 0.8
        world.mobs(i) += delta
      }
    }
  }

  def draw(): Unit = {
    ctx.clear()
    ctx.push(viewMat) {
      ctx.fillStyle = "blue"
      ctx.fillPath { ctx.circle(world.player, 4) }
      if ((world.player -> mouseWorldPos).length >= 4) {
        ctx.strokeStyle = "blue"
        ctx.strokePath { ctx.circle(world.player, 12) }
        val pointer = world.player + (world.player -> mouseWorldPos).normed * 12
        ctx.fillPath { ctx.circle(pointer, 2) }
      }
      ctx.strokeStyle = "grey"
      ctx.lineWidth = 3
      if (attacking) {
        ctx.strokePath {
          ctx.moveTo(sword.a)
          ctx.lineTo(sword.b)
        }
      } else {
        ctx.strokePath {
          val base = world.player + (world.player -> mouseWorldPos).normed.perp * 3
          val tip = base + (world.player -> mouseWorldPos).normed * 4
          ctx.moveTo(base)
          ctx.lineTo(tip)
        }
      }

      ctx.fillStyle = "orange"
      for (mob <- world.mobs) {
        ctx.fillPath { ctx.circle(mob, 4) }
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
