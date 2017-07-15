package pcgtest

import kit._
import org.scalajs.dom.html

class Boxes(page: AABB, seed: Int) {
  val margins = page.shrink(100)

  def boxes(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{d, fill, height, stroke, width}
    import scalatags.JsDom.svgTags.{g, path, svg}

    val size = 20
    val nx = (margins.width / size).floor.toInt
    val ny = (margins.height / size).floor.toInt
    val rects = for (y <- 0 until ny; x <- 0 until nx) yield {
      val d = Math.sqrt((x - (nx/2)) * (x - (nx/2)) + (y - ny/2) * (y - ny/2)) / 2
      val tl = Vec2(x, y) * size + Rand.withinCircle(d)
      val br = tl + Vec2(size, size) * (1.0/(0.1 + math.pow(math.max(d, 1), 0.5))) + Rand.withinCircle(d)
      if (br.x < tl.x || br.y < tl.y)
        AABB(Vec2(0, 0), Vec2(0, 0))
      else
        AABB(tl, br)
    }
    def render(): Unit = {
      val e = svg(
        width := page.width,
        height := page.height,
        g(
          rects.map { r =>
            path(d := r.toPolygon.translate(margins.lower).toSVG, fill := "transparent", stroke := "black")
          }: _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
    render()
  }

}
