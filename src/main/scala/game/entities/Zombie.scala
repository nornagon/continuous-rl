package game.entities

import game.{Assets, DynamicEntity, Entity, World}
import kit.{Circle2, Shape2, Vec2}
import org.scalajs.dom._
import kit.CanvasHelpers._
import kit.cp.Implicits._


class Zombie extends DynamicEntity {
  override def mass = 10.0
  def update(world: World, dt: Double): Unit = {
    val towardsPlayer = (pos -> world.player.pos).normed
    body.applyImpulse(towardsPlayer * 100 * dt, Vec2(0, 0))
    body.a = (pos -> world.player.pos).toAngle
  }

  override def didMount(world: World): Unit = {
    body.a = (pos -> world.player.pos).toAngle
  }

  override def shape: Seq[Shape2] = Seq(Circle2(Vec2(0,0), 10))

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.at(pos, body.a) {
      ctx.drawImageCentered(Assets.zombie1, Vec2(0, 0), 0.5)
    }
  }

  override def hit(world: World, other: Entity): Unit = {
    other match {
      case p: Player =>
        world.gameOver = true
      case _ =>
    }
  }
}
