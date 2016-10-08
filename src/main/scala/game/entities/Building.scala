package game.entities

import game.StaticEntity
import kit.Polygon


class Building(outline: Polygon) extends StaticEntity {
  def shape = Seq(outline)
}

