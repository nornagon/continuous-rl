package pcgtest

import kit._
import org.scalajs.dom.html

class Waves(page: AABB, seed: Int) {

  def waves(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{attr, d, fill, height, stroke, viewBox, width, xmlns}
    import scalatags.JsDom.svgTags.{g, path, svg}

    val margins = page.shrink(100)

    @Tweakable.Options
    case class Params(
                       n: Int = 200,
                       @Tweakable.Range(0, Math.PI)
                       rotAmount: Double = 2.4,
                       stickLength: Double = 100
                     )

    def render(options: Params) = {
      import options._
      val centerLine = Segment2(
        Vec2(margins.lower.x, margins.center.y),
        Vec2(margins.upper.x, margins.center.y)
      )

      val segs = (1 to (n - 1)) map { i =>
        val rot = Mat33.rotate(-math.cos(i * 2 * Math.PI / n) * rotAmount)
        val up = rot * Vec2(0, stickLength)
        Segment2(centerLine.sample(i.toDouble / n) + up, centerLine.sample(i.toDouble / n) - up)
      }

      val paths: Seq[String] = for (s <- segs; if margins.contains(s); seg <- margins.truncate(s)) yield seg.toSVG

      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        attr("x-seed") := s"$seed",
        g(
          paths.map { p => path(d := p, stroke := "black", fill := "transparent") }: _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }

    Params.tweakable()(render _)
  }
}
