package game.entities

import game.actions.MoveAction
import game.{Assets, DynamicEntity, World}
import kit.{Circle2, Shape2, Vec2}
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import kit.cp.Implicits._


class Player extends DynamicEntity {
  override def mass = 10.0
  var ammo = 6
  var pose = Assets.blueManGun
  def update(world: World, dt: Double): Unit = {
    world.currentAction match {
      case Some(action) =>
        if (!action.isInstanceOf[MoveAction])
          body.applyImpulse(-body.getVel() * 50 * dt, Vec2(0, 0))
      case _ =>
    }
  }

  override def shape: Seq[Shape2] = Seq(Circle2(Vec2(0,0), 10))

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.at(pos, body.a) {
      ctx.drawImageCentered(pose, Vec2(0, 0), 0.5)
    }
  }
}

