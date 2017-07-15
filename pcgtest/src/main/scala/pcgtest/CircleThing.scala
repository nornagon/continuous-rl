package pcgtest

import kit.{AABB, Circle2, Segment2, Tweakable}
import org.scalajs.dom.html

import kit.RandomImplicits._
import scalatags.JsDom.implicits._
import scalatags.JsDom.svgAttrs.{attr, d, height, viewBox, width, xmlns}
import scalatags.JsDom.svgTags.{g, path, svg}

class CircleThing(page: AABB, seed: Int) {
  def circle(root: html.Div): Unit = {
    val margins = page.shrink(50)

    @Tweakable.Options
    case class Params(
      @Tweakable.Range(2, 100)
      nPoints: Int,
      @Tweakable.Range(2, 100)
      takeNPoints: Int,
      @Tweakable.Range(2, 100)
      radius: Int,
      @Tweakable.Range(2, 40)
      gridX: Int = 20,
      @Tweakable.Range(2, 40)
      gridY: Int = 10,
      @Tweakable.Range(0.1, 40)
      freq: Double = 1,
      @Tweakable.Range(2, 4000)
      noiseX: Int = 20,
      @Tweakable.Range(2, 4000)
      noiseY: Int = 10
    )

    def render(params: Params) = {
      import params._
      implicit var r = new scala.util.Random(seed)

      val noise = new kit.pcg.Noise(seed)

      val points = margins.subdivided(gridX, gridY)
      val circles = points.map(c => Circle2(c, radius * noise.simplex2(c.x / noiseX, c.y / noiseY) * r.between(0.8, 1.2)).toPolygon(nPoints))
      /*val lines = points.sliding(2).flatMap {
        case Seq(a, b) =>
          r.shuffle(Circle2(a, radius).toPolygon(nPoints).points).take(takeNPoints).zip(r.shuffle(Circle2(b, radius).toPolygon(nPoints).points).take(takeNPoints)).map {
            case (p1, p2) => Segment2(p1, p2)
          }
      }.toList.filter(_.length < 300)*/


      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        attr("x-seed") := s"$seed",
        g(circles.filter(margins.contains).map { c =>
          path(d := c.toSVG)
        }: _*)/*,
        g(lines.map { c =>
          path(d := c.toSVG)
        }: _*)*/
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }

    Params.tweakable(7, 4, 10)(render _)
  }
}
