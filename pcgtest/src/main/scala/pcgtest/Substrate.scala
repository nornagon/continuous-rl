package pcgtest

import kit.{AABB, Rand, Vec2}
import kit.CanvasHelpers._
import kit.pcg.SubstrateOptions
import org.scalajs.dom
import org.scalajs.dom.html

class Substrate(page: AABB, seed: Int) {

  def substrate(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
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
          /*for (i <- 0 to s.deadSegments.size) {
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
          }*/
        }
      }
    }, 1/60.0)
    def stop(): Unit = dom.window.clearInterval(ivl)
    ivl
  }

  def substrateSVG(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{d, fill, height, stroke, viewBox, width, xmlns}
    import scalatags.JsDom.svgTags.{g, path, svg}

    val margins = page.shrink(100)
    val s = new kit.pcg.Substrate(
      //sources = Set((Vec2(0, 0), 1d), (Vec2(100, -20), 2d)),
      sources = ((1 to Rand.between(3, 7)) map { _ => (Rand.withinCircle(margins.minDimension / 2), Rand.angle) }).toSet,
      SubstrateOptions(
        chooseNewSegmentPosition = { parent =>
          val t = Rand.between(0, parent.length)
          val start = parent.a + (parent.a -> parent.b).normed * t
          val angle = (parent.a -> parent.b).toAngle + Rand.chooseFrom(-Math.PI/2 -> 1d, Math.PI/2 -> 1d, Rand.angle -> 10.09)
          (start, angle)
        },
        maxRadius = margins.minDimension / 2,
        changeDirectionChance = 0.0,
        //changeDirectionAmount = 0.9,
        changeDirectionMinSegLength = 200
        //maxSegmentLength = 100
      )
    )
    //s.deadSegments.append(Segment2(Vec2.forAngle(0) * margins.minDimension / 2, Vec2.forAngle(Math.PI) * margins.minDimension / 2))
    //s.deadSegments.append(Segment2(Vec2.forAngle(0.5) * margins.minDimension / 2, Vec2.forAngle(Math.PI+0.5) * margins.minDimension / 2))
    while (s.liveSegments.nonEmpty && (s.deadSegments.size + s.liveSegments.size) <= 900)
      s.step()
    def render(): Unit = {
      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        g(
          //transform := s"translate(${page.width / 2},${page.height / 2})",
          g(
            s.allSegments.filter(_.length >= 5).map(_.translate(Vec2(page.width/2, page.height/2))).flatMap(margins.truncate(_)).map { s =>
              path(d := s.toSVG, fill := "transparent", stroke := "black")
            }: _*
          )
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
    render()
  }
}
