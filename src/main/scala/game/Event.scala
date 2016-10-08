package game

import game.entities.{Splatter, Zombie}
import kit.cp


trait Event { def applyTo(world: World): Unit }

case class BulletHit(e: Entity, info: cp.SegmentQueryInfo) extends Event {
  override def applyTo(world: World): Unit = {
    e match {
      case z: Zombie =>
        world.removeEntity(z)
        world.addEntity(new Splatter, z.pos)
      case _ =>
    }
  }
}

