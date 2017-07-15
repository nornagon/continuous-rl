package game.entities

import game.{DynamicEntity, Incorporeal, World}
import kit.{Rand, Vec2}
import org.scalajs.dom.CanvasRenderingContext2D


class ZombieSpawner extends DynamicEntity with Incorporeal {
  override def update(world: World, dt: Double): Unit = {
    if (Math.random() < 0.1 * dt) {
      val pos = world.player.pos + Vec2.forAngle(Rand.angle) * Rand.between(500, 1500)
      world.addEntity(new Zombie, pos)
    }
  }

  override def draw(ctx: CanvasRenderingContext2D): Unit = {}
}
