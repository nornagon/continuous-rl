package pcgtest

import kit.RandomImplicits._
import kit._
import org.scalajs.dom.html

class Circles(page: AABB, seed: Int) {


  def circles(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{attr, d, fill, height, stroke, viewBox, width, xmlns}
    import scalatags.JsDom.svgTags.{g, path, svg}

    val margins = page.shrink(100)

    @Tweakable.Options
    case class Options(
                        @Tweakable.Range(-5, 5)
                        offsetX: Double = 0,
                        @Tweakable.Range(1, 500)
                        numCircles: Int = 100,
                        @Tweakable.Range(1, 50)
                        numCenters: Int = 3,
                        @Tweakable.Range(0, 50)
                        spacing: Double = 3,
                        @Tweakable.Range(0, 100)
                        wiggle: Double = 0,
                        @Tweakable.Range(0, 360)
                        wobble: Double = 0,
                        aSeed: Int = seed
                      )

    def render(options: Options): Unit = {
      import options._
      val r = new scala.util.Random(aSeed)
      val centers = for (_ <- 1 to numCenters) yield r.withinAABB(margins)
      val circles = centers flatMap (center => {
        val ang = r.angle
        for (i <- 1 to numCircles) yield {
          Circle2(
            center + Vec2(offsetX, 0) * i + Vec2.forAngle(i * wobble * 2*Math.PI/360 + ang) * wiggle,
            i * spacing
          )
        }
      })

      val paths = circles.map(margins.truncate).flatMap {
        case None => Seq.empty
        case Some(Left(c)) => Seq(c.toSVG)
        case Some(Right(arcs)) => arcs.map(_.toSVG)
      }

      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        attr("x-seed") := s"$aSeed",
        g(
          paths.map { p => path(d := p, stroke := "black", fill := "transparent") }: _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }

    Options.tweakable()(render _)
  }
}
