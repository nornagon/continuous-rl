package pcgtest

import kit.CanvasHelpers._
import kit.RandomImplicits._
import kit._
import kit.pcg.LloydRelaxation
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}

class Voronoi(page: AABB, seed: Int) {
  def voronoiSVG(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgAttrs.{attr, d, fill, height, stroke, viewBox, width, xmlns}
    import scalatags.JsDom.svgTags.{g, path, svg}
    val margins = page.shrink(50)
    implicit var r = new scala.util.Random(seed)

    //- Definitions -//
    def randomPoints(n: Int = 200) =
      for (_ <- 1 to n) yield Vec2(r.nextDouble() * page.width, r.nextDouble() * page.height)
    def cropCircles(numPoints: Int = 1300, relax: Boolean = true) = {
      val points = randomPoints(numPoints)
      val relaxed = if (relax) LloydRelaxation.voronoiRelax(margins, points).toSeq else points
      relaxed ++ (1 to 5 flatMap { _ =>
        val radius = r.between(20, 300)
        Circle2(r.withinAABB(margins), radius).toPolygon((radius * r.between(0.5, 0.7)).round.toInt).points
      })
    }
    def justCircles(numCircles: Int = 19, relax: Boolean = false, density: Double = 0.5) = {
      val points = 1 to numCircles flatMap { _ =>
        val radius = r.between(60, 300)
        Circle2(r.withinAABB(margins), radius).toPolygon((radius * (density * r.between(0.9, 1.1))).round.toInt).points
      }
      if (relax) LloydRelaxation.voronoiRelax(margins, points)
      else points
    }
    def flower() =
      (for (i <- 1 to r.between(5, 8)) yield {
        val jitter = r.between(0, 5)
        Circle2(margins.center, r.between(4, margins.minDimension / 2)).toPolygon(r.between(2, 80)).points.map { p => p + r.withinCircle(jitter) }
      }).flatten
    def hexes(size: Double = 14.0) = {
      val height = size * 2
      val vert = height * 0.75
      val width = math.sqrt(3)/2 * height
      for (y <- 0 to (margins.height / vert).toInt + 1; x <- 0 to (margins.width / width).toInt + 1) yield margins.lower + Vec2(x * width + (y % 2) * width/2, y * vert)
    }

    def shiftCells(cells: Seq[Polygon], shiftFactor: Vec2 => Double): Seq[Polygon] = {
      cells map { c => c.translate(r.withinCircle(shiftFactor(c.centroid))) }
    }
    def rotateCells(cells: Seq[Polygon], rotateFactor: Vec2 => Double): Seq[Polygon] = {
      cells map { c => c.translate(-c.centroid).rotateAroundOrigin(rotateFactor(c.centroid)).translate(c.centroid) }
    }
    def shiftCellsOutwards(cells: Seq[Polygon]) = shiftCells(cells, c => math.pow((c -> margins.center).length/200, 2.5))
    def shiftCellsRightwards(cells: Seq[Polygon]) = shiftCells(cells, c => math.pow(c.x / 400, 2.5))
    def shiftCellsRandomly(cells: Seq[Polygon]) = shiftCells(cells, c => 5)
    def rotateCellsRandomly(cells: Seq[Polygon]) = rotateCells(cells, c => r.between(-0.1, 0.1))
    def rotateCellsOutwardsRandomly(cells: Seq[Polygon]) = rotateCells(cells, c => r.between(-1.0, 1.0) * math.pow((c -> margins.center).length/600, 2.5))
    def rotateCellsOutwards(cells: Seq[Polygon]) = rotateCells(cells, c => math.pow((c -> margins.center).length/400, 1.2))

    def perturbPoints(points: Seq[Vec2], factor: Vec2 => Double) = points map { p => p + r.withinCircle(factor(p)) }
    def excludePoints(points: Seq[Vec2], circle: Circle2): Seq[Vec2] = points.filterNot(circle.contains)

    def relax[T <: Iterable[Vec2]](points: T, n: Int = 1): Seq[Vec2] =
      Iterator.iterate(points.toSeq)(ps =>
        LloydRelaxation.voronoiRelax(margins, ps).toSeq
      ).drop(n).next()

    @Tweakable.Options
    case class Params(
      @Tweakable.Enum("justCircles", "cropCircles", "hexes", "perturbedHexes")
      initial: String = "cropCircles",
      @Tweakable.Range(0, 50)
      initialSize: Int = 10,
      @Tweakable.Range(0, 2)
      density: Double = 0.4,
      doPreRelax: Boolean = true,
      @Tweakable.Range(0, 5)
      postRelaxCount: Int = 4,
      @Tweakable.Range(10, 100)
      hexSize: Int = 43,
      doRandomlyConnect: Boolean = false,
      doShiftCellsOutwards: Boolean = false
    )
    def render(params: Params) = {
      import params._
      r = new scala.util.Random(seed)
      //- Options -//
      //val points = justCircles(relax = true)
      //val points = cropCircles(numPoints = 500, relax = false)
      //val points = relax(justCircles(initialSize, relax=doPreRelax) ++ hexes(hexSize), n=postRelaxCount)
      //val points = justCircles(10, relax=false, density=0.4) ++ perturbPoints(hexes(20), x => 1 + (margins.center -> x).length/20)
      var points = params.initial match {
        case "justCircles" => justCircles(initialSize, relax = doPreRelax, density = density)
        case "cropCircles" => cropCircles(numPoints = 500, relax = doPreRelax)
        case "hexes" => justCircles(initialSize, relax=doPreRelax, density = density) ++ hexes(hexSize)
        case "perturbedHexes" =>
          justCircles(initialSize, relax = doPreRelax, density = density) ++ perturbPoints(hexes(hexSize), x => 1 + (margins.center -> x).length/20)
      }
      points = relax(points, n=postRelaxCount)
      val useCells = false

      val initialCells = Voronoi.computeD3(points) filter (margins contains _)

      /*val v = new Voronoi(points)
      while (v.nextEventY.isDefined) v.step()*/
      //val cells = shiftCellsRightwards(initialCells)
      //val cells = rotateCellsOutwards(initialCells)
      //val cells = rotateCellsOutwardsRandomly(initialCells)
      var cells = initialCells
      if (doShiftCellsOutwards)
        cells = shiftCellsOutwards(cells)

      val segs =
        if (doRandomlyConnect)
          cells filter (margins.shrink(40).contains(_)) flatMap (c => randomlyConnect(c.points, factor = 0.2))
        else
          cells flatMap (_.segments)

      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        attr("x-seed") := s"$seed",
        g(
          (
            if (useCells)
              (for (cell <- cells; if margins.shrink(40).contains(cell); seg <- margins.truncate(cell)) yield {
                path(d := seg.toSVG, fill := "transparent", stroke := "black")
              })(collection.breakOut)
            else
              (for (seg <- segs; if margins.contains(seg)) yield {
                path(d := seg.toSVG, fill := "transparent", stroke := "black")
              })(collection.breakOut)
            ): _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
    Params.tweakable()(render _)
  }

  def voronoi(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val width = 512d
    val height = 512d
    val margin = 32d
    val r = new scala.util.Random(44)
    val points = for (i <- 1 to 200) yield Vec2(r.nextDouble() * width + margin, r.nextDouble() * height + margin)
    //val points = Seq(Vec2(50, 30), Vec2(70, 60), Vec2(80, 80))
    /*val points = Circle2(Vec2(width, height) * 0.5 + Vec2(margin, margin), (width min height) * 0.4).toPolygon(70).points.map { p =>
      p + Vec2(r.nextDouble(), r.nextDouble())
    }*/

    var mousePos: Option[Vec2] = None
    dom.window.onmousemove = (e: MouseEvent) => {
      mousePos = Some(Vec2(e.clientX, e.clientY))
      draw()
    }

    def draw(): Unit = {
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      val relevantMousePos = mousePos.filter(AABB(0, 0, width + margin * 2, Double.PositiveInfinity).contains(_))
      val directrix = relevantMousePos.map(_.y).getOrElse(0.0)
      val v = new kit.Voronoi(points)
      var prevY = 0.0d
      while (v.nextEventY.exists(_ < directrix)) {
        val d = v.nextEventY.get
        /*if (v.beaches.nonEmpty)
          for (dy <- prevY to d by 1; (lb, rb) <- v.beaches.zip(v.beaches.tail)) {
            val x = v.leftBreakPoint(lb.site, rb.site, dy)
            val y = 0.5 * ((lb.site.x - x) * (lb.site.x - x) / (lb.site.y - dy) + lb.site.y + dy)
            ctx.fillStyle = "black"; ctx.fillRect(x, y, 1, 1)
          }*/
        prevY = d
        v.step()
      }
      /*if (v.beaches.nonEmpty)
        for (dy <- prevY to directrix by 1; (lb, rb) <- v.beaches.zip(v.beaches.tail)) {
          val x = v.leftBreakPoint(lb.site, rb.site, dy)
          val y = 0.5 * ((lb.site.x - x) * (lb.site.x - x) / (lb.site.y - dy) + lb.site.y + dy)
          ctx.fillStyle = "black"; ctx.fillRect(x, y, 1, 1)
        }*/
      for (p <- points) {
        ctx.fillPath("blue") { ctx.circle(p, 1) }
      }
      val colors = 0 to 10 map (i => s"hsl(${i/10.0*360}, ${if (i%2==0) 50 else 100}%, 50%)")
      val circledBeaches = v.circles().map(_._1)
      for ((b, i) <- v.beaches.zipWithIndex) {
        if (b.site.y == directrix) {
          ctx.strokePath(colors(i % colors.size)) { ctx.moveTo(b.site); ctx.lineTo(b.site.x, 0)  }
        } else {
          val rightmost = if (i == v.beaches.size - 1) Double.PositiveInfinity else v.leftBreakPoint(b.site, v.beaches(i+1).site, directrix)
          val leftmost = if (i == 0) Double.NegativeInfinity else v.leftBreakPoint(v.beaches(i-1).site, b.site, directrix)
          val parabolaPoints =
            for (x <- math.max(0, leftmost) to math.min(width + margin * 2, rightmost) by 1) yield {
              // y = ((a-x)^2 / (b-k) + (b + k))/2
              // where (a,b) is the focus and y=k is the directrix
              val y = 0.5 * ((b.site.x - x) * (b.site.x - x) / (b.site.y - directrix) + b.site.y + directrix)
              Vec2(x, y)
            }
          if (parabolaPoints.nonEmpty)
            ctx.strokePath(if (circledBeaches.contains(b)) "red" else colors(i % colors.size)) { ctx.polyLine(parabolaPoints) }
        }
      }
      for ((_, c) <- v.circles()) {
        ctx.strokePath("black") { ctx.circle(c) }
      }
      for ((_, e) <- v.edges) {
        if (e.start != null && e.end != null)
          ctx.strokePath("green") { ctx.polyLine(Seq(e.start, e.end)) }
      }
      for (vertex <- v.vertices) {
        ctx.strokePath("red") {
          ctx.moveTo(vertex + Vec2(-2, -2))
          ctx.lineTo(vertex + Vec2(2, 2))
          ctx.moveTo(vertex + Vec2(2, -2))
          ctx.lineTo(vertex + Vec2(-2, 2))
        }
      }
      ctx.strokePath("red") { ctx.moveTo(0, directrix + 0.5); ctx.lineTo(width + margin * 2, directrix + 0.5) }
      for ((Seq(lx_, rx_), i) <- v.beachProjections(directrix).sliding(2).filter(_.size == 2).zipWithIndex) {
        val lx = math.max(0, lx_)
        val rx = math.min(width + margin * 2, rx_)
        if (rx > lx)
          ctx.fillPath(colors(i % colors.size)) { ctx.rect(lx, directrix + 1, rx - lx, 5) }
      }
    }
    draw()
  }
}
