package game.entities

import game.{Assets, Layer, StaticEntity}
import kit._
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._


class Tree() extends StaticEntity {
  def shape = Seq(Circle2(Vec2(0, 0), 5).toPolygon(numPoints = 3))

  override val layer: Int = Layer.Top

  override def castsShadow: Boolean = true

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.at(pos, body.a) {
      ctx.drawImageCentered(Assets.tree, Vec2(0, 0))
    }
  }
}

