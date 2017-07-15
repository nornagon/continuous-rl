package game.actions

import game.{PlayerAction, World}
import kit.Vec2
import kit.cp.Implicits._


case class MoveAction(angle: Double, speed: Double) extends PlayerAction {
  override def update(world: World, dt: Double): Unit = {
    world.player.setTargetAngle(angle)
    val vel: Vec2 = world.player.body.getVel()
    val facing = Vec2.forAngle(world.player.body.a)
    if (vel.length > 0.1) {
      val sin = (vel cross facing) / vel.length
      world.player.body.applyImpulse(-vel * sin.abs, Vec2(0, 0))
    }
    world.player.body.applyImpulse(Vec2.forAngle(angle) * speed, Vec2(0, 0))
  }

  override def isDone: Boolean = true
}
/*
case class MoveAction(dir: Vec2) extends PlayerAction {
  override def update(world: World, dt: Double): Unit = {
    val vel: Vec2 = world.player.body.getVel()
    val slow = if (vel.length > 0.1) {
      val sint = (vel cross dir) / (vel.length * dir.length)
      -vel * sint.abs * 0.5
    } else Vec2(0, 0)
    val imp = Vec2.forAngle((dir + slow).toAngle) * dir.length
    world.player.body.applyImpulse(imp * dt, Vec2(0, 0))
  }

  override def isDone: Boolean = true
}

*/
