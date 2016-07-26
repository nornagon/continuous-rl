package kit

import monix.reactive.subjects.BehaviorSubject
import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js


/**
  * A window-sized &lt;canvas&gt; with the appropriate pixel ratio.
  */
class Canvas {
  val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
  val ctx = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  element.style.display = "block"

  def width = dom.window.innerWidth
  def height = dom.window.innerHeight

  val resizes = BehaviorSubject((dom.window.innerWidth, dom.window.innerHeight))

  val onWindowResize: js.Function1[dom.Event, Unit] = { (e: dom.Event) =>
    val w = width
    val h = height
    element.width = math.round(w * dom.window.devicePixelRatio).toInt
    element.height = math.round(h * dom.window.devicePixelRatio).toInt
    element.style.width = s"${w}px"
    element.style.height = s"${h}px"
    ctx.scale(dom.window.devicePixelRatio, dom.window.devicePixelRatio)
    resizes.onNext((w, h))
    ()
  }

  onWindowResize(null)
  dom.window.addEventListener("resize", onWindowResize)

  def unmount() = {
    // TODO: mutation observers?
    dom.window.removeEventListener("resize", onWindowResize)
  }
}
