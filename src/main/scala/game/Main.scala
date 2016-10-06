package game

import kit._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom._
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import kit.CanvasHelpers._
import scala.scalajs.js
import kit.cp.Implicits._
import js.JSConverters._

class World {
  //var player = Vec2(0, 0)
  var ammo = 6
  //var mobs = mutable.Seq(Vec2(100, 100), Vec2(100, 300))
  val mobs = mutable.Buffer.empty[cp.Body]
  var trees = mutable.Seq.fill(20) {
    (Vec2(Math.random() * 1000 - 500, Math.random() * 1000 - 500), Math.random() * Math.PI / 2)
  }
  var splatters = mutable.Seq[(Vec2, Double)]()
  val buildings: mutable.Buffer[Seq[Vec2]] = mutable.Buffer.empty

  def addBuilding(b: Polygon): Unit = {
    buildings += b.toPolyLine
    space.addStaticShape(new cp.PolyShape(space.staticBody, b.toCCWPolyLine.flatMap(p => Seq(p.x, p.y)).toJSArray, Vec2(0, 0)))
  }

  def addMob(p: Vec2): Unit = {
    val body = new cp.Body(m = 5, cp.Cp.momentForCircle(m = 5, r1 = 0, r2 = 10, Vec2(0, 0)))
    val shape = new cp.CircleShape(body, radius = 10, Vec2(0, 0))
    shape.setCollisionType(2)
    space.addBody(body)
    space.addShape(shape)
    body.setPos(p)
    mobs += body
  }

  def removeMob(mob: cp.Body): Unit = {
    space.removeBody(mob)
    for (s <- mob.shapeList)
      space.removeShape(s)
    mobs.remove(mobs.indexOf(mob))
  }

  val space = new cp.Space
  space.damping = 0.5
  space.gravity = Vec2(0, 0)
  val playerBody = new cp.Body(m = 9, cp.Cp.momentForCircle(m = 9, r1 = 0, r2 = 10, Vec2(0, 0)))
  private val shape = new cp.CircleShape(playerBody, radius = 10, Vec2(0, 0))
  shape.setElasticity(0.1)
  playerBody.addShape(shape)
  space.addBody(playerBody)
  space.addShape(shape)
  shape.setCollisionType(1)
  def player: Vec2 = playerBody.getPos()
  for ((t, a) <- trees) {
    val treeShape = new cp.PolyShape(
      space.staticBody,
      Circle2(t, 5).toPolygon(3, a).toPolyLine.reverse.flatMap(p => Seq(p.x, p.y)).toJSArray,
      Vec2(0, 0)
    )
    space.addStaticShape(treeShape)
  }
  var gameOver = false
  def zHitP(arb: cp.Arbiter, sp: cp.Space): Unit = {
    gameOver = true
  }
  space.addCollisionHandler(1, 2, null, zHitP _, null)
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
      val deadMobs = world.mobs.filter { mob => bullet intersects Circle2(mob.getPos(), 10) }
      world.splatters ++= deadMobs.map(mob => (mob.getPos(): Vec2, Math.random() * Math.PI * 2))
      for (mob <- deadMobs)
        world.removeMob(mob)
      //world.mobs = world.mobs.filterNot { mob => bullet intersects Circle2(mob.getPos(), 10) }
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

  val debugVectors = mutable.Buffer.empty[(Segment2, String)]

  case class MoveAction(dir: Vec2) extends Action {
    override def init(world: World): Unit = {
      //world.player += dir
      val vel: Vec2 = world.playerBody.getVel()
      val slow = if (vel.length > 0.1) {
        val sint = (vel cross dir) / (vel.length * dir.length)
        -vel * sint.abs * 0.5
      } else Vec2(0, 0)
      val imp = Vec2.forAngle((dir + slow).toAngle) * dir.length
      /*debugVectors += ((Segment2(world.playerBody.getPos(), world.playerBody.getPos() + imp * 5), "red"))
      debugVectors += ((Segment2(world.playerBody.getPos(), world.playerBody.getPos() + dir * 5), "green"))
      debugVectors += ((Segment2(world.playerBody.getPos(), world.playerBody.getPos() + slow * 5), "orange"))*/
      world.playerBody.applyImpulse(imp, Vec2(0, 0))
    }
    def done(world: World): Boolean = true
  }

  def input(): Option[Action] = {
    if ((codesDown contains "Space") && world.ammo > 0)
      Some(ShootAction(Vec2.forAngle((world.player -> mouseWorldPos).toAngle + ((Math.random() * 2 - 1) * 0.1))))
    else if (codesDown contains "KeyR")
      Some(ReloadAction())
    else if (codesDown contains "KeyW")
      Some(MoveAction((world.player -> mouseWorldPos).normed * (if (keysDown contains KeyCode.Shift) 30 else 10)))
    else if (codesDown contains "KeyS")
      Some(MoveAction(-(world.player -> mouseWorldPos).normed * 5))
    else
      None
  }

  var currentAction: Option[Action] = None
  var buildMode = false
  var buildPoints = Seq.empty[Vec2]

  def update(): Unit = {
    if (world.gameOver) {
      dom.window.alert("The zombie's grip is tight as death. Its teeth bite down on your flesh and you realise it's the end...")
      world = new World
      keysDown.clear()
      codesDown.clear()
      mouseDown = false
      currentAction = None
    }
    debugVectors.clear()
    currentAction match {
      case None =>
        currentAction = input()
        currentAction foreach { newAction => newAction.init(world) }
      case Some(action) =>
        action.update(world)
    }
    if (codesDown contains "KeyB") {
      buildMode = true
      buildPoints = Seq.empty
    }
    if (codesDown contains "Escape") {
      buildMode = false
    }
    if (buildMode) {
      if (events exists (_.isInstanceOf[MouseDown]))
        buildPoints :+= mouseWorldPos
      if (buildPoints.size == 4) {
        world.addBuilding(Polygon(buildPoints))
        buildMode = false
      }
    }
    if (currentAction.isDefined) {
      if (!currentAction.exists(_.isInstanceOf[MoveAction]))
        world.playerBody.applyImpulse(-world.playerBody.getVel(), Vec2(0,0))
      if (Math.random() < 0.01) {
        world.addMob(Vec2(Math.random() * ctx.canvas.width - ctx.canvas.width / 2, Math.random() * ctx.canvas.height - ctx.canvas.height / 2))
      }
      for ((mob, i) <- world.mobs.zipWithIndex) {
        val nearby = world.mobs.zipWithIndex.filter(_._2 != i).map(_._1.getPos())
        def f(separation: Double) = Math.log(separation / 40) / separation
        val nbForce = nearby.map(mob.getPos() -> _).map(v => v.normed * f(v.length)).reduceOption(_ + _).getOrElse(Vec2(0,0))
        val playerForce = (mob.getPos() -> world.player).normed * 1
        val delta = (nbForce + playerForce).normed * 3
        //world.mobs(i) += delta
        world.mobs(i).applyImpulse(delta, Vec2(0, 0))
        world.mobs(i).resetForces()
      }
      world.space.step(1.0 / 60)
    }
    currentAction foreach { act =>
      if (act.done(world))
        currentAction = None
    }
    viewMat = Mat33.translate(ctx.canvas.width / 2, ctx.canvas.height / 2) * Mat33.translate(-world.player.x, -world.player.y)
  }

  val numShadowCasts = 1
  val shadowcastColor = f"rgba(0, 0, 0, ${if (numShadowCasts == 1) 1 else Math.pow(0.9, numShadowCasts)}%.2f)"

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
          ctx.translate(mob.getPos().x, mob.getPos().y)
          ctx.rotate((mob.getPos() -> world.player).toAngle)
          ctx.drawImage(Assets.zombie1, 0, 0, Assets.zombie1.width, Assets.zombie1.height, -Assets.zombie1.width / 4, -Assets.zombie1.height / 4, Assets.zombie1.width / 2, Assets.zombie1.height / 2)
        }
      }

      for ((tree, angle) <- world.trees) {
        ctx.save {
          ctx.translate(tree.x, tree.y)
          ctx.rotate(angle)
          ctx.drawImage(Assets.tree, 0, 0, Assets.tree.width, Assets.tree.height, -Assets.tree.width / 2, -Assets.tree.height / 2, Assets.tree.width, Assets.tree.height)
        }
      }

      for (b <- world.buildings)
        ctx.strokePath("orange", lineWidth = 8) { ctx.polygon(b) }

      val treePolys = for ((tree, a) <- world.trees) yield
        Circle2(tree, 5).toPolygon(numPoints = 3, startAngle = a).toPolyLine
      val walls = treePolys ++ world.buildings
      if (numShadowCasts > 0) {
        if (numShadowCasts > 1)
          ctx.globalCompositeOperation = "multiply"
        ctx.fillStyle = shadowcastColor
        for (p <- Vec2.aroundCircle(numPoints = numShadowCasts, startAngle = (world.player -> mouseWorldPos).toAngle)) {
          val fov = FOV.calculateFOV(
            world.player + p * 2,
            walls,
            bounds = AABB(world.player - Vec2(1000, 1000), world.player + Vec2(1000, 1000))
          )
          ctx.fillPathEvenOdd {
            ctx.rect(world.player.x + -1000, world.player.y + -1000, 2000, 2000)
            ctx.polyLine(fov)
          }
        }
        ctx.globalCompositeOperation = "source-over"
      }
      if (true) {
        val grad = ctx.createRadialGradient(world.player.x, world.player.y, 10, world.player.x, world.player.y, 1000)
        val time = 19
        if (time > 8 && time < 19) {
          grad.addColorStop(0, "hsla(0,0%,100%,0)")
          grad.addColorStop(0.5, "hsla(42,75%,74%,0.2)")
        } else if (time >= 19 && time < 21) {
          grad.addColorStop(0, "hsla(0,0%,0%,0.0)")
          grad.addColorStop(0.7, "hsla(0,0%,0%,0.95)")
        } else {
          grad.addColorStop(0, "hsla(0,0%,0%,0.5)")
          grad.addColorStop(0.2, "hsla(0,0%,0%,1.0)")
        }
        ctx.fillStyle = grad
        ctx.fillRect(world.player.x - 1000, world.player.y - 1000, 2000, 2000)
      }
      if (buildMode) {
        ctx.strokePath("aquamarine") { ctx.polygon(buildPoints :+ mouseWorldPos) }
        ctx.fillPath("aquamarine") { ctx.circle(mouseWorldPos, 5) }
        ctx.strokePath("aquamarine") { ctx.circle(mouseWorldPos, 10) }
      }
      for ((s, c) <- debugVectors) {
        ctx.strokePath(c) { ctx.polyLine(Seq(s.a, s.b)) }
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

  trait KEvent
  case class MouseDown(p: Vec2) extends KEvent

  val events = mutable.Buffer[KEvent]()
  def frame(t: Double): Unit = {
    update()
    draw()
    events.clear()
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
    dom.window.addEventListener("mousedown", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY); mouseDown = true; events.append(MouseDown(mousePos)) })
    dom.window.addEventListener("mouseup", (e: MouseEvent) => { mousePos = Vec2(e.clientX, e.clientY); mouseDown = false })
    run()
  }
}
