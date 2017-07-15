package pcgtest

import kit.pcg.LloydRelaxation
import kit.{AABB, Rand, Vec2}
import kit.CanvasHelpers._
import org.scalajs.dom
import org.scalajs.dom.{KeyboardEvent, html}

class Relaxation {

  def relaxation(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val bounds = AABB(Vec2(0, 0), Vec2(ctx.canvas.width, ctx.canvas.height))
    var points: Seq[Vec2] = for (_ <- 1 to 1000) yield Rand.withinAABB(bounds)
    def render(): Unit = {
      ctx.fillStyle = "rgba(255, 255, 255, 0.8)"
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height)
      for (p <- points) {
        ctx.fillPath("black") { ctx.circle(p, 2) }
      }
    }
    render()
    dom.window.onkeydown = (e: KeyboardEvent) => {
      println(e.keyCode)
      if (e.keyCode == 0x20) {
        points = LloydRelaxation.monteCarloRelax(bounds, points).toSeq
        println(points.size)
        render()
      }
    }
  }
}
