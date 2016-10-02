package game

import kit.{Circle2, Mat33, Segment2, Vec2}
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom._
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import kit.CanvasHelpers._
import scala.scalajs.js


class World {
  var player = Vec2(0, 0)
  var ammo = 6
  var mobs = mutable.Seq(Vec2(100, 100), Vec2(100, 300))
  var trees = mutable.Seq.fill(20)((Vec2(Math.random() * 1000 - 500, Math.random() * 1000 - 500), Math.random() * Math.PI / 2))
  var splatters = mutable.Seq[(Vec2, Double)]()
}

object Assets {
  private def img(src: String) = {
    val i = dom.document.createElement("img").asInstanceOf[dom.raw.HTMLImageElement]
    i.src = src
    i
  }
  val tree = img("assets/Tiles/tile_183.png")
  val blueManGun = img("assets/Man Blue/manBlue_gun.png")
  val blueManReload = img("assets/Man Blue/manBlue_reload.png")
  val zombie1 = img("assets/Zombie 1/zoimbie1_hold.png")
  val splatter = img("assets/Tiles/tile_320.png")

  val square = img("assets/squareWhite.png")
}

@JSExport
object Main {
  var world = new World
  var ctx: dom.CanvasRenderingContext2D = _
  val keysDown = mutable.Set[Int]()
  val codesDown = mutable.Set[String]()
  var mousePos: Vec2 = Vec2(0, 0)
  def mouseWorldPos: Vec2 = viewMat.inverse * mousePos
  var mouseDown: Boolean = false
  var viewMat: Mat33 = _


  trait Action {
    def init(world: World): Unit = {}
    def update(world: World): Unit = {}
    def done(world: World): Boolean
    def draw(ctx: dom.CanvasRenderingContext2D, world: World): Unit = {}
  }

  case class ShootAction(direction: Vec2) extends Action {
    val attackLength = 15
    val range = 600
    var t: Double = 0
    var base: Vec2 = _

    def bullet: Segment2 = {
      val perFrame = range / attackLength.toDouble
      val dist = t / attackLength * range
      Segment2(base + direction * dist, base + direction * (dist+perFrame))
    }
    override def init(world: World): Unit = {
      base = world.player + direction.perp * 5 + direction * 7
      world.ammo -= 1
    }
    override def update(world: World): Unit = {
      val deadMobs = world.mobs.filter { mob => bullet intersects Circle2(mob, 10) }
      world.splatters ++= deadMobs.map((_, Math.random() * Math.PI * 2))
      world.mobs = world.mobs.filterNot { mob => bullet intersects Circle2(mob, 10) }
      t += 1
    }
    def done(world: World): Boolean = {
      t > attackLength
    }

    override def draw(ctx: CanvasRenderingContext2D, world: World): Unit = {
      ctx.strokeStyle = "lightgray"
      ctx.lineWidth = 3
      ctx.strokePath {
        ctx.moveTo(bullet.a)
        ctx.lineTo(bullet.b)
      }
    }
  }

  case class ReloadAction() extends Action {
    var t: Double = 0
    override def update(world: World): Unit = {
      t += 1
      if (done(world))
        world.ammo = 6
    }
    def done(world: World): Boolean = {
      t > 120
    }
  }

  case class MoveAction(dir: Vec2) extends Action {
    override def init(world: World): Unit = {
      world.player += dir
    }
    def done(world: World): Boolean = true
  }

  def input(): Option[Action] = {
    if ((codesDown contains "Space") && world.ammo > 0)
      Some(ShootAction(Vec2.forAngle((world.player -> mouseWorldPos).toAngle + ((Math.random() * 2 - 1) * 0.1))))
    else if (codesDown contains "KeyR")
      Some(ReloadAction())
    else if (mouseDown || (codesDown contains "KeyW"))
      Some(MoveAction((world.player -> mouseWorldPos).normed * (if (keysDown contains KeyCode.Shift) 3 else 1)))
    else
      None
  }

  var currentAction: Option[Action] = None

  def update(): Unit = {
    currentAction match {
      case None =>
        currentAction = input()
        currentAction foreach { newAction => newAction.init(world) }
      case Some(action) =>
        action.update(world)
    }
    if (currentAction.isDefined) {
      if (Math.random() < 0.01) {
        world.mobs :+= Vec2(Math.random() * ctx.canvas.width - ctx.canvas.width / 2, Math.random() * ctx.canvas.height - ctx.canvas.height / 2)
      }
      for ((mob, i) <- world.mobs.zipWithIndex) {
        val nearby = world.mobs.zipWithIndex.filter(_._2 != i).map(_._1)
        def f(separation: Double) = Math.log(separation / 40) / separation
        val nbForce = nearby.map(mob -> _).map(v => v.normed * f(v.length)).reduceOption(_ + _).getOrElse(Vec2(0,0))
        val playerForce = (mob -> world.player).normed * 1
        val delta = (nbForce + playerForce).normed * 0.8
        world.mobs(i) += delta
      }
      for (mob <- world.mobs) {
        if (Circle2(mob, 10) intersects Circle2(world.player, 10)) {
          dom.window.alert("The zombie's grip is tight as death. Its teeth bite down on your flesh and you realise it's the end...")
          world = new World
          keysDown.clear()
          codesDown.clear()
          mouseDown = false
          currentAction = None
        }
      }
    }
    currentAction foreach { act =>
      if (act.done(world))
        currentAction = None
    }
    viewMat = Mat33.translate(ctx.canvas.width / 2, ctx.canvas.height / 2) * Mat33.translate(-world.player.x, -world.player.y)
  }

  def draw(): Unit = {
    ctx.fillStyle = "hsl(145, 63%, 42%)"
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    ctx.push(viewMat) {
      for ((s, a) <- world.splatters) {
        ctx.save {
          ctx.translate(s.x, s.y)
          ctx.rotate(a)
          ctx.drawImage(Assets.splatter, 0, 0, Assets.splatter.width, Assets.splatter.height, -Assets.splatter.width / 4, -Assets.splatter.height / 4, Assets.splatter.width / 2, Assets.splatter.height / 2)
        }
      }
      ctx.save {
        ctx.translate(world.player.x, world.player.y)
        ctx.rotate((world.player -> mouseWorldPos).toAngle)
        val sprite = if (currentAction exists (_.isInstanceOf[ReloadAction])) Assets.blueManReload else Assets.blueManGun
        ctx.drawImage(sprite, 0, 0, sprite.width, sprite.height, -sprite.width / 4, -sprite.height / 4, sprite.width / 2, sprite.height / 2)
      }

      for (act <- currentAction)
        act.draw(ctx, world)

      for (mob <- world.mobs) {
        ctx.save {
          ctx.translate(mob.x, mob.y)
          ctx.rotate((mob -> world.player).toAngle)
          ctx.drawImage(Assets.zombie1, 0, 0, Assets.zombie1.width, Assets.zombie1.height, -Assets.zombie1.width / 4, -Assets.zombie1.height / 4, Assets.zombie1.width / 2, Assets.zombie1.height / 2)
        }
      }

      for ((tree, angle) <- world.trees) {
        ctx.save {
          ctx.rotate(angle)
          ctx.drawImage(Assets.tree, 0, 0, Assets.tree.width, Assets.tree.height, tree.x - Assets.tree.width / 2, tree.y - Assets.tree.height / 2, Assets.tree.width, Assets.tree.height)
        }
      }
    }
    for (i <- 0 until world.ammo) {
      ctx.drawImage(Assets.square, 0, 0, Assets.square.width, Assets.square.height,
        ctx.canvas.width - Assets.square.width - 10 - (Assets.square.width + 5) * i,
        10,
        Assets.square.width,
        Assets.square.height
      )
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
    //element.style.cursor = "none"
    root.appendChild(element)
    ctx = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    viewMat = Mat33.translate(ctx.canvas.width / 2, ctx.canvas.height / 2)
    dom.window.addEventListener("keydown", (e: KeyboardEvent) => {
      codesDown += e.asInstanceOf[js.Dynamic].code.asInstanceOf[String]
      keysDown += e.keyCode
    })
    dom.window.addEventListener("keyup", (e: KeyboardEvent) => {
      codesDown -= e.asInstanceOf[js.Dynamic].code.asInstanceOf[String]
      keysDown -= e.keyCode
    })
    dom.window.addEventListener("blur", (e: FocusEvent) => { keysDown.clear(); codesDown.clear(); mouseDown = false })
    dom.window.addEventListener("mousemove", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY) })
    dom.window.addEventListener("mousedown", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY); mouseDown = true })
    dom.window.addEventListener("mouseup", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY); mouseDown = false })
    run()
  }
}
