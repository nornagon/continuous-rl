package game

import org.scalajs.dom.{CanvasRenderingContext2D, html}
import scala.scalajs.js.annotation.JSExport


@JSExport
object Main {
  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    val canvas = new kit.Canvas(draw)
    root.appendChild(canvas.canvas)
  }

  def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    ctx.fillStyle = "red"
    ctx.fillRect(10, 10, ctx.canvas.width - 20, ctx.canvas.height - 20)
  }
}
