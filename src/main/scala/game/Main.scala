package game

import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExport


@JSExport
object Main {
  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.width = math.round(dom.window.innerWidth * dom.window.devicePixelRatio).toInt
    canvas.height = math.round(dom.window.innerHeight * dom.window.devicePixelRatio).toInt
    canvas.style.width = s"${dom.window.innerWidth}px"
    canvas.style.height = s"${dom.window.innerHeight}px"
    canvas.style.display = "block"
    root.appendChild(canvas)
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    ctx.scale(dom.window.devicePixelRatio, dom.window.devicePixelRatio)
    ctx.fillStyle = "red"
    ctx.fillRect(10, 20, 30, 40)
  }
}
