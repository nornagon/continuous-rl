package pcgtest

import kit._
import org.scalajs.dom.html
import kit.cp.Implicits._

class Time(page: AABB, seed: Int) {
  def time(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{d, fill, height, stroke, transform, width}
    import scalatags.JsDom.svgTags.{g, path, svg}
    val space = new cp.Space
    space.setIterations(100)
    space.gravity = Vec2(0, 10)
    val bounds = AABB(Vec2(0, 0), Vec2(100, 100))
    val boxes = for (i <- 1 to 10) yield {
      val body = Iterator.continually {
        val w = Rand.between(3, 20)
        val h = Rand.between(3, 20)
        val body = new cp.Body(10, cp.Cp.momentForBox(10, w, h))
        body.setPos(Rand.withinAABB(bounds.shrink(20)))
        body.a = Rand.angle
        val shape = cp.BoxShape(body, w, h)
        body.addShape(shape)
        body
      }.find { body =>
        !space.shapeQuery(body.shapeList(0), null)
      }.get

      space.addBody(body)
      body.shapeList.map(space.addShape)
      body
    }
    for (seg <- bounds.toPolygon.segments) {
      space.addShape(new cp.SegmentShape(space.staticBody, seg.a, seg.b, 1))
    }
    def renderGroup() = {
      g(
        boxes map { b =>
          val s = b.shapeList.head
          s match {
            case c: cp.PolyShape =>
              val poly = Polygon(c.verts.grouped(2).map { a => Vec2(a(0), a(1)) }.toSeq).rotateAroundOrigin(-b.a).translate(b.getPos())
              path(
                d := poly.toSVG,
                fill := "transparent",
                stroke := "black"
              )
          }
        }: _*
      )
    }
    def render() = {
      val numX = (1100 / bounds.width).floor.toInt
      val numY = (850 / bounds.height).floor.toInt
      val numSteps = numX * numY
      val groups = for (i <- 0 until numSteps) yield {
        for (_ <- 1 to 9)
          space.step(1 / 120.0)
        val x = i % numX
        val y = (i / numX).floor
        renderGroup()(transform := s"translate(${x * bounds.width}, ${y * bounds.height})")
      }
      val e = svg(
        width := 1100,
        height := 850,
        g(groups: _*)
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
    render()
  }

}
