package kit

import org.scalajs.dom

case class ShapeProps[Msg](
  fill: String = null,
  stroke: String = null,
  id: String = null,
  onMouseDown: () => Msg = null,
  onMouseOver: () => Msg = null,
  onMouseOut: () => Msg = null
)

trait Shaper[Msg] {
  def shape(props: ShapeProps[Msg])(d: dom.CanvasRenderingContext2D => Unit): Unit
  def rect(x: Double, y: Double, width: Double, height: Double, props: ShapeProps[Msg]): Unit =
    shape(props) { _.rect(x, y, width, height) }
}

class RenderShaper[Msg](ctx: dom.CanvasRenderingContext2D) extends Shaper[Msg] {
  def shape(props: ShapeProps[Msg])(d: dom.CanvasRenderingContext2D => Unit): Unit = {
    ctx.beginPath()
    d(ctx)
    if (props.fill != null) {
      ctx.fillStyle = props.fill
      ctx.fill()
    }
  }
}

class HitTestShaper[Msg](ctx: dom.CanvasRenderingContext2D, x: Double, y: Double) extends Shaper[Msg] {
  var lastHit: ShapeProps[Msg] = _
  override def shape(props: ShapeProps[Msg])(d: (dom.CanvasRenderingContext2D) => Unit): Unit = {
    if (props.id != null) {
      ctx.beginPath()
      d(ctx)
      if (ctx.isPointInPath(x * dom.window.devicePixelRatio, y * dom.window.devicePixelRatio))
        lastHit = props
    }
  }
}
