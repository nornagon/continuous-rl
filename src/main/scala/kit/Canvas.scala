package kit

import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js


/**
  * A window-sized &lt;canvas&gt; with the appropriate pixel ratio.
  */
class Canvas(invalidate: dom.CanvasRenderingContext2D => Unit) {
  val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  ctx.scale(dom.window.devicePixelRatio, dom.window.devicePixelRatio)
  canvas.style.display = "block"

  def width = dom.window.innerWidth
  def height = dom.window.innerHeight

  val onWindowResize: js.Function1[dom.Event, Unit] = { (e: dom.Event) =>
    canvas.width = math.round(width * dom.window.devicePixelRatio).toInt
    canvas.height = math.round(height * dom.window.devicePixelRatio).toInt
    canvas.style.width = s"${width}px"
    canvas.style.height = s"${height}px"
    invalidate(ctx)
  }

  onWindowResize(null)
  dom.window.addEventListener("resize", onWindowResize)

  def unmount() = {
    // TODO: mutation observers?
    dom.window.removeEventListener("resize", onWindowResize)
  }
}
