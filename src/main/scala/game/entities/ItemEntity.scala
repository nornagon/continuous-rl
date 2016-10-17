package game.entities

import game.StaticEntity
import game.items.Item
import kit.{Circle2, Shape2, Vec2}


class ItemEntity(val item: Item) extends StaticEntity {
  override def shape: Seq[Shape2] = Seq(Circle2(Vec2(0, 0), 2))
}
