package pcgtest

import kit.RandomImplicits._
import kit.{AABB, Mat33, Segment2, Vec2}
import org.scalajs.dom.html

class InterlockingGrids(page: AABB, seed: Int) {

  def interlockingGrids(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{attr, d, fill, height, stroke, viewBox, width, xmlns}
    import scalatags.JsDom.svgTags.{g, path, svg}
    implicit val r = new scala.util.Random(seed)

    val margins = page.shrink(100)

    val cellSize = 80
    val numCellsX = (margins.width / cellSize).floor.toInt
    val numCellsY = (margins.height / cellSize).floor.toInt
    val spacing = 5
    val numSegs = (cellSize / spacing).floor.toInt
    val segs = (-numSegs/2-25 to numSegs/2+25) map { i => Segment2(Vec2(-cellSize, i*spacing), Vec2(cellSize, i*spacing)) }
    val cells = for (y <- 0 until numCellsY; x <- 0 until numCellsX) yield {
      AABB(x * cellSize, y * cellSize, (x + 1) * cellSize, (y + 1) * cellSize).translate(margins.lower)
    }
    //val cells = Seq(AABB(margins.center - Vec2(cellSize/2, cellSize/2), margins.center + Vec2(cellSize/2, cellSize/2)))
    val segments = (for (cell <- cells) yield {
      //val cell = AABB(x * cellSize, y * cellSize, (x + 1) * cellSize, (y + 1) * cellSize).translate(margins.lower)
      (for (i <- 1 to 1) yield {
        val mat = Mat33.rotate(r.angle)
        val rotatedSegs = segs.map { s => Segment2(mat * s.a + cell.center, mat * s.b + cell.center) }
        val truncatedSegs = rotatedSegs.flatMap(cell.truncate(_))
        val points = truncatedSegs flatMap (seg => Seq(seg.a, seg.b))
        randomlyConnect(points, 0.03 * math.cos((cell.center.x - margins.center.x) / margins.width * 4)) filter { seg => seg.a.x != seg.b.x && seg.a.y != seg.b.y}
        //truncatedSegs // ++ cell.toPolygon.segments
      }).flatten
    }).flatten

    val alternatedSegments = segments.zipWithIndex map { case (seg, i) => if (i % 2 == 0) seg.reverse else seg }

    val e = svg(
      xmlns := "http://www.w3.org/2000/svg",
      width := s"${page.width / 100.0}in",
      height := s"${page.height / 100.0}in",
      viewBox := s"0 0 ${page.width} ${page.height}",
      attr("x-seed") := s"$seed",
      g(
        (for (shape <- alternatedSegments; s = shape) yield { //s <- margins.truncate(shape)) yield {
          path(d := s.toSVG, fill := "transparent", stroke := "black")
        })(collection.breakOut): _*
      )
    ).render
    root.innerHTML = ""
    root.appendChild(e)
  }
}
