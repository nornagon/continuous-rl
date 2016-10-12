package game.actions

import game.{PlayerAction, World}
import kit.{Segment2, Vec2, cp}
import kit.cp.Implicits._


case class GrabAction() extends PlayerAction {
  override def update(world: World, dt: Double): Unit = {
    val segment = Segment2(
      world.player.rightHandPos,
      world.player.rightHandPos + Vec2.forAngle(world.player.pointingAngle) * 10
    )
    val hitInfo = world.space.segmentQueryFirst(segment.a, segment.b, cp.Cp.ALL_LAYERS, 0)
    if (hitInfo != null) {
      val hitPos = segment.a.lerp(segment.b, hitInfo.t)
      world.player.grab(world, hitInfo.shape.getBody(), (hitPos - hitInfo.shape.getBody().getPos()).rotate(hitInfo.shape.getBody().a))
    } else {
      world.player.ungrab(world)
    }
  }

  override def isDone: Boolean = true
}
