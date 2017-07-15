package pcgtest

import kit._
import kit.RandomImplicits._
import org.scalajs.dom.html

class Arcs(page: AABB, seed: Int) {

  def arcs(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{attr, d, fill, height, stroke, transform, viewBox, width, xmlns}
    import scalatags.JsDom.svgTags.{g, path, svg}

    val r = new scala.util.Random(seed)

    val margins = page.shrink(100)

    val cellSize: Double = 100
    val numCellsX: Int = math.floor(margins.width / cellSize).toInt
    val numCellsY: Int = math.floor(margins.height / cellSize).toInt

    val largeArc = Arc2(
      Vec2(0, 0),
      cellSize,
      cellSize,
      0,
      0,
      Math.PI / 2
    ).scale(0.7).translate(-Vec2(cellSize, cellSize) / 2)
    val smallArc = Arc2(
      Vec2(0, 0),
      cellSize,
      cellSize,
      0,
      0,
      Math.PI / 2
    ).scale(0.3).translate(-Vec2(cellSize, cellSize) / 2)
    val grid = new pcg.WaveFunctionCollapse().wfc(numCellsX, numCellsY)
    val paths = (for (x <- 0 until numCellsX; y <- 0 until numCellsY) yield {
      //val angle = r.between(0, 4) * Math.PI / 2
      if (grid((x, y)) == " ") {
        Seq()
      } else {
        val angle = (grid((x, y)) match {
          case "┘" => 0
          case "┐" => 1
          case "┌" => 2
          case "└" => 3
        }) * Math.PI / 2
        val cellCenter = Vec2(cellSize * (x + 0.5), cellSize * (y + 0.5))
        val arc = largeArc.rotateAboutOrigin(angle).translate(cellCenter)
        val arc2 = smallArc.rotateAboutOrigin(angle).translate(cellCenter)
        val nRays = 5
        val rays = arc.toPoints(nRays).zip(arc2.toPoints(nRays)).map { case (p1, p2) =>
          Segment2(p1, p2)
        }
        val nJoins = 5
        val joins = rays.map(_.toPoints(nJoins + r.between(-1, 1))).sliding(2).filter(_.size == 2).flatMap {
          case Seq(a, b) => a.zip(b.reverse).map { case (p1, p2) => Segment2(p1, p2) }
        }
        joins
        /*Seq(
          smallArc.rotateAboutOrigin(angle).translate(cellCenter),
          largeArc.rotateAboutOrigin(angle).translate(cellCenter)
        )*/
      }
    }).flatten.map(_.toSVG)

    val e = svg(
      xmlns := "http://www.w3.org/2000/svg",
      width := s"${page.width / 100.0}in",
      height := s"${page.height / 100.0}in",
      viewBox := s"0 0 ${page.width} ${page.height}",
      attr("x-seed") := s"$seed",
      g(
        transform := s"translate(${margins.lower.x}, ${margins.lower.y})",
        g(
          paths.map { p =>
            path(d := p, stroke := "black", fill := "transparent")
          }: _*
        )
      )
    ).render
    root.innerHTML = ""
    root.appendChild(e)
  }
}
