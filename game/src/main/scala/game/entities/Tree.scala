package game.entities

import game.{Assets, RenderLayer, StaticEntity}
import kit._
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._


class Tree(radius: Double = 8) extends StaticEntity {
  val shape = Seq(Circle2(Vec2(0, 0), radius).toPolygon(numPoints = 6))

  override val renderLayer: Int = RenderLayer.Top

  override def castsShadow: Boolean = true

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.at(pos, body.a) {
      val s = math.sqrt(radius / 8)
      ctx.scale(s, s)
      ctx.drawImageCentered(Assets.tree, Vec2(0, 0))
    }
  }
}

