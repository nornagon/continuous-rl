package game

import game.entities.{Bullet, Player}
import game.items.Gun
import kit._
import org.scalajs.dom.CanvasRenderingContext2D
import scala.collection.mutable
import kit.CanvasHelpers._
import kit.cp.Implicits._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


class World {
  val player: Player = new Player
  val dynamicEntities = mutable.Buffer.empty[DynamicEntity]
  val staticEntities = mutable.Buffer.empty[StaticEntity]
  val space = new cp.Space
  space.damping = 0.5
  space.setDefaultCollisionHandler(null, null, handleCollision _, null)
  var gameOver = false

  addEntity(player, Vec2(0, 0))

  var currentAction: Option[PlayerAction] = None

  val frameEvents = mutable.Buffer.empty[Event]
  val deadEntities = mutable.Buffer.empty[Entity]

  var codesDown = Set.empty[String]
  var mouseScreenPos = Vec2(0, 0)
  var screenSize = Vec2(0, 0)

  def viewMatrix = Mat33.translate(screenSize / 2) * Mat33.translate(-player.pos)
  def mousePos = viewMatrix.inverse * mouseScreenPos

  def processInput(): Unit = {
    val pointingDelta = Math.max(-0.4, Math.min(0.4, Angle.clipToPi((player.pos -> mousePos).toAngle - player.body.a)))
    player.pointingAngle = player.body.a + pointingDelta
    if (currentAction.isDefined) return
    currentAction =
      if (codesDown contains "KeyW") {
        val speed = if (codesDown contains "ShiftLeft") 20 else 10
        Some(actions.MoveAction((player.pos -> mousePos).toAngle, speed))
      } else if (codesDown contains "KeyF") {
        Some(actions.GrabAction())
      } else if (codesDown contains "KeyS") {
        Some(actions.MoveAction((player.pos -> mousePos).toAngle, -4))
      } else if (codesDown contains "KeyR") {
        player.held.flatMap(_.component[items.Gun]) match {
          case Some(gun) =>
            Some(actions.ReloadAction(gun))
          case None =>
            None
        }
      } else if (codesDown contains "KeyD") {
        player.held match {
          case Some(item) =>
            player.held = None
            addEntity(new entities.ItemEntity(item), player.pos)
            Some(actions.PauseAction(0.1))
          case None =>
            None
        }
      } else if (codesDown contains "KeyG") {
        val itemsHere = entitiesWithin(Circle2(player.pos, 20)).collect { case ie: entities.ItemEntity => ie }
        if (itemsHere.nonEmpty) {
          val closest = itemsHere.minBy { ie => (player.pos -> ie.pos).lengthSquared }
          removeEntity(closest)
          player.held = Some(closest.item)
          Some(actions.PauseAction(0.2))
        } else None
      } else if (codesDown contains "Space") {
        player.held.flatMap(_.component[items.Gun]) match {
          case Some(gun) =>
            if (gun.ammo > 0) {
              gun.ammo -= 1
              val firingAngle = player.pointingAngle + (Math.random() * 2 - 1) * 0.1
              val base = player.rightHandPos
              addEntity(new Bullet(firingAngle, 0.1, 4800), base)
              Some(actions.PauseAction(0.2))
            } else None
          case None => None
        }
      } else None
  }

  def handleCollision(arbiter: cp.Arbiter, space: cp.Space): Unit = {
    val a = entityForBody(arbiter.getA().getBody())
    val b = entityForBody(arbiter.getB().getBody())
    a.hit(this, b)
    b.hit(this, a)
  }

  def update(): Unit = {
    val dt = 1.0/60 * 2
    currentAction match {
      case Some(action) =>
        deadEntities foreach doRemoveEntity
        deadEntities.clear()
        action.update(this, dt)

        frameEvents.clear()
        space.step(dt)

        dynamicEntities foreach (_.update(this, dt))
        frameEvents foreach (_.applyTo(this))

        if (action.isDone)
          currentAction = None
      case _ =>
    }
  }

  def entitiesWithin(aabb: AABB): Seq[Entity] = {
    val entities = mutable.Set.empty[Entity]

    def check(s: cp.Shape): Unit = {
      if (aabb.intersects(s.getBB()))
        entities += entityForBody(s.getBody())
    }

    // Query the BBTrees directly instead of through bbQuery so we can ignore layers/groups, otherwise roads don't get drawn
    // TODO: use a separate (non-cpSpace) BBTree for tracking non-colliding static shapes?
    space.staticShapes.query(aabb, check _)
    space.activeShapes.query(aabb, check _)

    // Add shapeless entities (e.g. bullets, splatters) according to their body locations
    // TODO: represent these in a BBTree somehow?
    for (e <- staticEntities ++ dynamicEntities; if e.body.shapeList.length == 0; if aabb.contains(e.pos))
      entities += e

    entities.toSeq
  }

  def entitiesWithin(c: Circle2): Seq[Entity] = {
    val entities = mutable.Set.empty[Entity]
    val aabb = c.boundingBox
    val qShape = new cp.CircleShape(null, c.r, c.c)
    def check(s: cp.Shape): Unit = {
      if (aabb.intersects(s.getBB())) {
        val contacts = cp.collideShapes(qShape, s)
        if (contacts.length > 0)
          entities += entityForBody(s.getBody())
      }
    }
    space.staticShapes.query(aabb, check _)
    space.activeShapes.query(aabb, check _)

    // TODO: shapeless bodies?

    entities.toSeq
  }

  def draw(ctx: CanvasRenderingContext2D): Unit = {
    val viewBounds = AABB(player.pos - Vec2(1000, 1000), player.pos + Vec2(1000, 1000))

    val visibleEntities = this.entitiesWithin(viewBounds)

    ctx.fillStyle = "hsl(145, 63%, 42%)"
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    ctx.push(viewMatrix) {
      val entitiesByLayer = visibleEntities.groupBy(_.renderLayer)
      for (layer <- entitiesByLayer.keys.toSeq.sorted; entity <- entitiesByLayer(layer))
        entity.draw(ctx)
      drawVignette(ctx)
      drawFOV(ctx, viewBounds, visibleEntities)
    }
    player.held.flatMap(_.component[items.Gun]) match {
      case Some(gun) =>
        for (i <- 0 until gun.ammo) {
          ctx.drawImage(Assets.square, 0, 0, Assets.square.width, Assets.square.height,
            ctx.canvas.width - Assets.square.width - 10 - (Assets.square.width + 5) * i,
            10,
            Assets.square.width,
            Assets.square.height
          )
        }
      case None =>
    }
    ctx.save {
      ctx.fillStyle = "thistle"
      ctx.font = "10px Menlo, Consolas, monospace"
      ctx.textBaseline = "top"
      ctx.fillText(s"Drew ${visibleEntities.size} entities", 10, 10)
    }
  }

  def drawFOV(ctx: CanvasRenderingContext2D, viewBounds: AABB, visibleEntities: Seq[Entity]): Unit = {
    val shadowcastColor = "rgba(0, 0, 0, 1.0)"
    val shadowcastingEntities = visibleEntities filter (_.castsShadow)
    ctx.fillStyle = shadowcastColor
    val fov = FOV.calculateFOV2(
      player.pos,
      shadowcastingEntities flatMap { e =>
        e.shape.flatMap {
          case p: Polygon =>
            p.rotateAroundOrigin(-e.body.a).translate(e.pos).segments
        }
      },
      bounds = viewBounds
    )
    ctx.fillPathEvenOdd {
      ctx.rect(player.pos.x - 1000, player.pos.y - 1000, 2000, 2000)
      ctx.polyLine(fov)
    }
  }

  def drawVignette(ctx: CanvasRenderingContext2D): Unit = {
    val grad = ctx.createRadialGradient(player.pos.x, player.pos.y, 10, player.pos.x, player.pos.y, 1000)
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
    ctx.fillRect(player.pos.x - 1000, player.pos.y - 1000, 2000, 2000)
  }

  def addEntity(e: Entity, pos: Vec2): Entity = {
    e.body.setPos(pos)
    e match {
      case static: StaticEntity =>
        staticEntities.append(static)
      case dynamic: DynamicEntity =>
        dynamicEntities.append(dynamic)
        space.addBody(e.body)
    }
    for (s <- e.shape) {
      val shape = s match {
        case c: Circle2 =>
          new cp.CircleShape(e.body, c.r, c.c)
        case p: Polygon =>
          new cp.PolyShape(e.body, p.toCCWPolyLine.flatMap(v => Seq(v.x, v.y)).toJSArray, Vec2(0, 0))
        case s: Segment2 =>
          new cp.SegmentShape(e.body, s.a, s.b, r = 2.0 /* TODO */)
      }
      shape.setLayers(e.layers)
      space.addShape(shape)
    }
    e.didMount(this)
    e.body.asInstanceOf[js.Dynamic].entity = e.asInstanceOf[js.Any]
    e
  }
  def removeEntity(e: Entity): Unit = {
    deadEntities += e
  }
  private def doRemoveEntity(e: Entity): Unit = {
    e match {
      case static: StaticEntity =>
        staticEntities.remove(staticEntities.indexOf(static))
      case dynamic: DynamicEntity =>
        dynamicEntities.remove(dynamicEntities.indexOf(dynamic))
    }
    if (space.containsBody(e.body))
      space.removeBody(e.body)
    for (s <- e.body.shapeList)
      space.removeShape(s)
  }

  def entityForBody(body: cp.Body): Entity = {
    body.asInstanceOf[js.Dynamic].entity.asInstanceOf[Entity]
  }

  def dispatch(ev: Event): Unit = frameEvents.append(ev)
}

