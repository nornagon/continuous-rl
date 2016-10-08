package game.entities

import game.{Assets, Incorporeal, StaticEntity}
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import kit.Vec2


class Splatter extends StaticEntity with Incorporeal {
  val a = Math.random() * Math.PI * 2
  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.at(pos, a) {
      ctx.drawImageCentered(Assets.splatter, Vec2(0, 0), 0.5)
    }
  }
}

