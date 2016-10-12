package game.entities

import game.{Assets, Incorporeal, RenderLayer, StaticEntity}
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import kit.Vec2


class Splatter extends StaticEntity with Incorporeal {
  val a = Math.random() * Math.PI * 2

  override val renderLayer: Int = RenderLayer.Background

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.at(pos, a) {
      ctx.drawImageCentered(Assets.splatter, Vec2(0, 0), 0.5)
    }
  }
}

