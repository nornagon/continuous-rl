package game.entities

import game.{BulletHit, DynamicEntity, World}
import kit.Vec2
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import kit.cp.Implicits._


class Bullet(angle: Double, timeOfFlight: Double, speed: Double) extends DynamicEntity {
  def shape = Seq.empty
  override def mass = Double.PositiveInfinity
  var t = 0.0
  var lastPos: Vec2 = _
  var tail: Vec2 = _
  override def didMount(world: World): Unit = {
    tail = pos
    lastPos = pos
    body.setVel(Vec2.forAngle(angle) * speed)
  }
  def update(world: World, dt: Double): Unit = {
    val hit = world.space.segmentQueryFirst(lastPos, pos, 1, 0)
    if (hit != null) {
      lastPos = lastPos + (lastPos -> pos) * hit.t
      world.dispatch(BulletHit(world.entityForBody(hit.shape.getBody()), hit))
      world.removeEntity(this)
    } else {
      tail = lastPos
      lastPos = pos
      t += dt
      if (t >= timeOfFlight)
        world.removeEntity(this)
    }
  }

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.strokePath("lightgray", lineWidth = 3.0) {
      ctx.polyLine(Seq(tail, lastPos))
    }
  }
}

