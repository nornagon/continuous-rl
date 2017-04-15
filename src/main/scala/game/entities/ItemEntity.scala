package game.entities

import game.items.Item
import game.{DynamicEntity, World}
import kit.{Circle2, Shape2, Vec2}


class ItemEntity(val item: Item) extends DynamicEntity {
  override def shape: Seq[Shape2] = Seq(Circle2(Vec2(0, 0), 2))

  override def mass: Double = 1

  override def update(world: World, dt: Double): Unit = ()
}
