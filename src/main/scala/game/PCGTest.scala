package game

import kit.{Polygon, Rand, Segment2, Vec2}
import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExport
import kit.CanvasHelpers._


@JSExport
object PCGTest {
  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth
    element.height = dom.window.innerHeight
    element.style.display = "block"
    root.appendChild(element)
    val ctx = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val s = new kit.pcg.Substrate(Set((Vec2(0, 0), 1d), (Vec2(100, -20), 2d)))
    //s.liveSegments += Segment2(Vec2(-1000, 0), Vec2(1000, 0))
    //s.liveSegments += Segment2(Vec2(-300, -200), Vec2(200, 420))
    lazy val ivl: Int = dom.window.setInterval({ () =>
      s.step()
      ctx.fillStyle = "#eaeaea"
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height)
      ctx.save {
        ctx.translate(ctx.canvas.width / 2, ctx.canvas.height / 2)
        for (seg <- s.allSegments) {
          ctx.strokePath("black", lineWidth = 1) {
            ctx.moveTo(seg.a)
            ctx.lineTo(seg.b)
          }
        }
        if (s.liveSegments.isEmpty || (s.deadSegments.size + s.liveSegments.size) > 400) {
          stop()
          for (i <- 0 to s.deadSegments.size) {
            val seg = Rand.oneOf(s.deadSegments: _*)
            val t = Rand.between(0, seg.length / 5).floor * 5
            val pointOnRoad = seg.a + (seg.a -> seg.b).normed * t
            val pointNearRoad = pointOnRoad + (seg.a -> seg.b).perp.normed * Rand.oneOf(-1, 1) * 10
            val poly = Polygon.square(12).rotateAroundOrigin(-(seg.a -> seg.b).toAngle).translate(pointNearRoad)
            if (!s.allSegments.exists(poly intersects _)) {
              val smallerPoly = Polygon.square(8).rotateAroundOrigin(-(seg.a -> seg.b).toAngle).translate(pointNearRoad)
              ctx.fillPath("thistle") {
                ctx.polygon(smallerPoly)
              }
            }
          }
        }
      }
    }, 1/60.0)
    def stop(): Unit = dom.window.clearInterval(ivl)
    ivl
  }
}
