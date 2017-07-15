package game.entities

import game.{RenderLayer, StaticEntity}
import kit.{Segment2, Shape2}
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import scala.scalajs.js


class Road(seg: Segment2) extends StaticEntity {
  override def shape: Seq[Shape2] = Seq(seg.toRectangle(200))

  override val renderLayer: Int = RenderLayer.Background

  override def layers: Int = 0

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.strokePath("gray", lineWidth = 200) {
      ctx.moveTo(seg.a)
      ctx.lineTo(seg.b)
    }
    ctx.save {
      ctx.setLineDash(js.Array(60, 90))
      ctx.strokePath("yellow", lineWidth = 4) {
        ctx.moveTo(seg.a)
        ctx.lineTo(seg.b)
      }
    }
  }
}
