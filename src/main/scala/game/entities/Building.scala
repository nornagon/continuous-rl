package game.entities

import game.{StaticEntity, World}
import kit.Polygon


class Building(outline: Polygon) extends StaticEntity {
  def shape = Seq(outline)

  override def didMount(world: World): Unit = {
    body.shapeList.foreach { s => s.group = 3 }
  }

  override def castsShadow: Boolean = true
}

