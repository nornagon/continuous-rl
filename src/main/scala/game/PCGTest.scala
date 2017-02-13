package game

import kit._
import org.scalajs.dom
import org.scalajs.dom.{KeyboardEvent, MouseEvent, html}

import scala.scalajs.js.annotation.{JSExport, JSExportAll, ScalaJSDefined}
import kit.CanvasHelpers._
import kit.pcg.{LloydRelaxation, Noise, SpacePacking, SubstrateOptions}
import kit.cp.Implicits._

import scala.collection.mutable
import kit.RandomImplicits._
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.SVGPathElement

import scala.scalajs.js


object qs {
  import js.native
  import js.annotation.JSName
  @JSName("QuickSettings")
  @native
  object QuickSettings extends js.Object {
    def create(x: Int, y: Int, panelTitle: String): QuickSettings = native
  }

  @JSName("QuickSettings")
  @native
  class QuickSettings extends js.Object {
    def addBoolean(title: String, value: Boolean, callback: js.Function1[Boolean, Unit]): Unit = native
    def addButton(title: String, callback: js.Function0[Unit]): Unit = native
    def addNumber(title: String, min: Double, max: Double, value: Double, step: Double, callback: js.Function1[Double, Unit]): Unit = native
    def addRange(title: String, min: Double, max: Double, value: Double, step: Double, callback: js.Function1[Double, Unit]): Unit = native
    def destroy(): Unit = native

    def getValue(title: String): js.Any = native

    def setDraggable(draggable: Boolean): Unit = native
    def setCollapsible(collapsible: Boolean): Unit = native
    def collapse(): Unit = native
    def expand(): Unit = native
    def show(): Unit = native
    def hide(): Unit = native
  }
}

@JSExport
object PCGTest {
  def timed[T](label: String)(f: => T): T = {
    val start = dom.window.performance.now()
    val ret = f
    val end = dom.window.performance.now()
    val dur = end - start
    println(s"$label took $dur ms")
    ret
  }

  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    //circles(root)
    //text(root)
    //interlockingGrids(root)
    //substrateSVG(root)
    //withCanvas(root, substrate)
    //withCanvas(root, spacePacking)
    //withCanvas(root, relaxation)
    //time(root)
    //stars(root)
    //boxes(root)
    //withCanvas(root, noise)
    //withCanvas(root, particles)
    //particlesSVG(root)
    //withCanvas(root, voronoi)
    voronoiSVG(root)
    //arcs(root)
    //waves(root)
  }

  val page = AABB(Vec2(0, 0), Vec2(1200, 900))
  //val page = AABB(Vec2(0, 0), Vec2(1000, 700))

  def withCanvas(root: html.Div, f: html.Canvas => Unit): Unit = {
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth.toInt
    element.height = dom.window.innerHeight.toInt
    element.style.display = "block"
    root.appendChild(element)
    f(element)
  }

  val seed: Int = scala.util.Random.nextInt
  val r = new scala.util.Random(seed)

  def randomlyConnect(points: Seq[Vec2], factor: Double): Seq[Segment2] =
    for (a <- points; b <- points; if a != b; if r.nextDouble() < factor) yield Segment2(a, b)

  def circles(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, xmlns, viewBox, transform, attr}

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

  def waves(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, xmlns, viewBox, transform, attr}

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

  def arcs(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, xmlns, viewBox, transform, attr}

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

  def boxes(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, transform}

    val pageWidth = 1100
    val pageHeight = 850
    val size = 20
    val nx = pageWidth / size
    val ny = pageHeight / size
    val rects = for (y <- 0 until ny; x <- 0 until nx) yield {
      val d = Math.sqrt((x - (nx/2)) * (x - (nx/2)) + (y - ny/2) * (y - ny/2)) / 5
      val tl = Vec2(x, y) * size + Rand.withinCircle(d)
      val br = tl + Vec2(size, size) * (1.0/(0.1 + math.pow(math.max(d, 1), 0.3))) + Rand.withinCircle(d)
      if (br.x < tl.x || br.y < tl.y)
        AABB(Vec2(0, 0), Vec2(0, 0))
      else
        AABB(tl, br)
    }
    val bounds = AABB(Vec2(0, 0), Vec2(1100, 850))
    def render(): Unit = {
      val e = svg(
        width := bounds.width,
        height := bounds.height,
        g(
          rects.map { r =>
            path(d := r.toPolygon.toSVG, fill := "transparent", stroke := "black")
          }: _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
    render()
  }

  def stars(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, transform}

    def makeStar(outerRadius: Double): Polygon = {
      val goldenRatio = 1.618
      val innerRadius = outerRadius / (1 + goldenRatio)
      val outer = Circle2(Vec2(0, 0), outerRadius).toPolygon(5)
      val inner = Circle2(Vec2(0, 0), innerRadius).toPolygon(5, Math.PI * 2 / 10)
      Polygon(outer.points.zip(inner.points).flatMap { case (a, b) => Seq(a, b) })
    }
    val bounds = AABB(Vec2(0, 0), Vec2(1100, 850))
    val star = makeStar(10)
    val stars = mutable.Buffer.empty[Polygon]
    for (_ <- 1 to 1000) {
      val pos = Rand.withinAABB(bounds.shrink(20))
      val size = Rand.between(0.8, 3)
      val angle = Rand.angle
      val s = star.transform(Mat33.translate(pos) * Mat33.scale(size) * Mat33.rotate(angle))
      if (!stars.exists { o =>
        (o.aabb intersects s.aabb) && o.segments.exists(oseg => s.segments.exists(oseg intersects _))
      }) {
        stars += s
      }
    }
    def render(): Unit = {
      val layers = stars.grouped((stars.size / 3.0).ceil.toInt).toSeq
      val e = svg(
        width := bounds.width,
        height := bounds.height,
        g(
          layers map { l =>
            g(
              l.map { s =>
                path(d := s.toSVG, fill := "transparent", stroke := "black")
              }: _*
            )
          }: _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
    render()
  }

  def time(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, transform}
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

  case class Particle(var p: Vec2, var v: Vec2, maxTrailLength: Double = 200) {
    val trail = mutable.Buffer[Vec2](p)
    def step(dt: Double, field: Vec2 => Vec2): Unit = {
      p += v * dt
      v += field(p)
      trail.append(p)
    }
    def trailLength() = (for (Seq(a, b) <- trail.sliding(2)) yield (a-b).length).sum
  }
  def particlesSVG(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, transform}
    var particles = mutable.Buffer[Particle]()
    var longParticles = mutable.Buffer[Particle]()
    val screen = AABB(Vec2(0, 0), Vec2(1100, 850))
    //for (i <- 1 to 100) particles += Particle(Rand.withinAABB(screen), Rand.withinCircle())
    val center = Vec2(screen.width / 2.0, screen.height / 2.0)
    /*particles ++= Circle2(Vec2(0, 0), 1).toPolygon(100).points.map { v =>
      Particle(center + v * 40, v * 26)
    }*/
    //val aabb = AABB(Vec2(100, 0), Vec2(1000, 250))
    val aabb = screen.shrink(200)
    val smaller = AABB(Vec2(400, 200), Vec2(800, 400))
    val centers = LloydRelaxation.monteCarloRelax(aabb, (1 to 500) map { _ => Rand.withinAABB(aabb) })
    particles ++= centers map {c => Particle(c, Vec2(0, 0), maxTrailLength = 200)}
    longParticles ++= (1 to 64) map {_ => Particle(Rand.withinAABB(aabb), Vec2(0, 0), maxTrailLength = 500)}
    val noiseX = new Noise(4)
    val noiseY = new Noise(3)
    val s = 1000
    def field(p: Vec2) = Vec2(noiseX.simplex2(p.x / s, p.y / s), noiseY.simplex2(p.x / s + 1, p.y / s + 1)) * 0.2 + (p -> center).normed * 0.05
    def update(dt: Double): Unit = {
      for (p <- particles ++ longParticles; if p.trailLength() < p.maxTrailLength) {
        p.step(dt, field)
        p.p += p.v * dt
      }
      particles = particles.filter(p => p.trail.forall(screen.contains))
      longParticles = longParticles.filter(p => p.trail.forall(screen.contains))
    }
    for (_ <- 1 to 200) {
      update(5)
    }
    def pathForTrail(trail: Seq[Vec2]): String = {
      val h = trail.head
      val t = trail.tail
      s"M${h.x},${h.y} ${t.map(p => s"L${p.x},${p.y}").mkString(" ")}"
    }
    val e = svg(
      width := screen.width,
      height := screen.height,
      g(
        particles.map { p =>
          path(d := pathForTrail(p.trail), fill := "transparent", stroke := "black")
        }: _*
      ),
      g(
        longParticles.map { p =>
          path(d := pathForTrail(p.trail), fill := "transparent", stroke := "red")
        }: _*
      )
    ).render
    root.innerHTML = ""
    root.appendChild(e)
  }

  def voronoiSVG(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g, text}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, transform, viewBox, xmlns, attr}
    val margins = page.shrink(50)
    var r = new scala.util.Random(seed)

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

      val e = timed("make svg")(svg(
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
      ).render)
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
      val v = new Voronoi(points)
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


  def particles(canvas: html.Canvas): Unit = {
    case class ParticleM(var p: Vec2, var v: Vec2)
    val particles = mutable.Buffer[ParticleM]()
    val screen = AABB(Vec2(0, 0), Vec2(canvas.width, canvas.height))
    //for (i <- 1 to 100) particles += Particle(Rand.withinAABB(screen), Rand.withinCircle())
    particles ++= Circle2(Vec2(canvas.width / 2.0, canvas.height / 2.0), 100).toPolygon(100).points.map(v => ParticleM(v, Vec2(0, 0)))
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val noiseX = new Noise(0)
    val noiseY = new Noise(1)
    val s = 1000
    def update(dt: Double): Unit = {
      for (p <- particles) {
        p.p += p.v * dt
        p.v += Vec2(noiseX.simplex2(p.p.x / s, p.p.y / s), noiseY.simplex2(p.p.x / s, p.p.y / s))
      }
    }
    def render(): Unit = {
      for (p <- particles) {
        ctx.fillPath { ctx.circle(p.p, 1) }
      }
    }
    def frame(): Unit = {
      update(1)
      render()
    }
    dom.window.setInterval(frame _, 100)
  }

  def noise(canvas: html.Canvas): Unit = {
    val n = new Noise(0)
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val image = ctx.createImageData(ctx.canvas.width, ctx.canvas.height)
    val data = image.data
    for (y <- 0 until ctx.canvas.height; x <- 0 until ctx.canvas.width) {
      val d = n.simplex2(x/50.0, y/50.0) // in [-1,1]
      val v: Int = ((d+1)/2 * 256).toInt
      val cell = (x + y * ctx.canvas.width) * 4
      data(cell) = v
      data(cell + 1) = v
      data(cell + 2) = v
      data(cell + 3) = 255
    }
    ctx.putImageData(image, 0, 0)
  }

  def relaxation(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val bounds = AABB(Vec2(0, 0), Vec2(ctx.canvas.width, ctx.canvas.height))
    var points: Seq[Vec2] = for (_ <- 1 to 1000) yield Rand.withinAABB(bounds)
    def render(): Unit = {
      ctx.fillStyle = "rgba(255, 255, 255, 0.8)"
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height)
      for (p <- points) {
        ctx.fillPath("black") { ctx.circle(p, 2) }
      }
    }
    render()
    dom.window.onkeydown = (e: KeyboardEvent) => {
      println(e.keyCode)
      if (e.keyCode == 0x20) {
        points = LloydRelaxation.monteCarloRelax(bounds, points).toSeq
        println(points.size)
        render()
      }
    }
  }

  @JSExport
  def spacePacking(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.translate(dom.window.innerWidth / 2, dom.window.innerHeight / 2)

    val rooms = Set(
      SpacePacking.Room(
        Polygon.square(100),
        Seq(
          (Vec2(0, -50), -Math.PI/2),
          (Vec2(0, 50), Math.PI/2),
          (Vec2(-50, 0), Math.PI),
          (Vec2(50, 0), 0.0)
        )
      ),
      SpacePacking.Room(
        Polygon.rectangle(40, 80),
        Seq(
          (Vec2(0, -40), -Math.PI/2),
          (Vec2(0, 40), Math.PI/2)
        )
      ),
      SpacePacking.Room(
        Polygon.rectangle(200, 100),
        Seq(
          (Vec2(0, -50), -Math.PI/2),
          (Vec2(0, 50), Math.PI/2),
          (Vec2(-100, 0), Math.PI),
          (Vec2(100, 0), 0.0)
        )
      )
    )
    /*for (room <- rooms) {
      ctx.strokePath {
        ctx.polygon(room.outline)
        for (door <- room.doors) {
          ctx.moveTo(door._1)
          ctx.lineTo(door._1 + Vec2.forAngle(door._2) * 5)
        }
      }
    }*/
    val s = new SpacePacking(rooms, (Vec2(0, 0), 0))
    ctx.fillPath { ctx.circle(Vec2(0, 0), 2) }
    for (i <- 1 to 20) {
      s.step()
    }
    for (room <- s.placedRooms) {
      ctx.strokePath("black") { ctx.polygon(room) }
    }
    for (d <- s.openDoors) {
      ctx.strokePath("orange") {
        ctx.moveTo(d._1)
        ctx.lineTo(d._1 + Vec2.forAngle(d._2) * 5)
      }
    }
  }

  @JSExport
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
    import scalatags.JsDom.svgTags.{svg, path, g}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, viewBox, transform, xmlns}

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

  def interlockingGrids(root: html.Div): Unit = {
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags.{svg, path, g, text}
    import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, transform, viewBox, xmlns, attr}
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

  def text(root: html.Div): Unit = {
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    case class Glyph(path: String, width: Double, height: Double)
    val url = "assets/isocpeur.svg"
    val glyphMapFuture = Ajax.get(url).map { xhr =>
      val glyphs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ:1234567890."
      val glyphSVGParent = dom.document.createElement("div")
      dom.document.body.appendChild(glyphSVGParent)
      glyphSVGParent.innerHTML = xhr.responseText
      val glyphSVG = glyphSVGParent.getElementsByTagName("svg")(0).asInstanceOf[dom.Element]
      val paths = glyphSVG.getElementsByTagName("path").asInstanceOf[js.Dynamic]
      val glyphMap: Map[Char, Glyph] = (for (g <- glyphs) yield {
        val codepoint = g.toInt
        val glyph = paths.selectDynamic(f"glyph-$codepoint%04X").asInstanceOf[SVGPathElement]
        val bbEl: dom.raw.SVGLocatable =
          if (glyph.hasAttribute("x-bb")) {
            glyphSVG.querySelector(glyph.getAttribute("x-bb")).asInstanceOf[dom.raw.SVGLocatable]
          } else
            glyph
        val bb = bbEl.getBBox()
        val seg = glyph.createSVGPathSegMovetoRel(-bb.x, -bb.y)
        glyph.pathSegList.insertItemBefore(seg, 0)
        g -> Glyph(path=glyph.getAttribute("d"), width=bb.width, height=bb.height)
      })(collection.breakOut)
      dom.document.body.removeChild(glyphSVGParent)
      glyphMap ++ Map(" " -> Glyph("", 20, 0))
    }
    val margins = page.shrink(100)
    for (glyphMap <- glyphMapFuture) {
      import scalatags.JsDom.implicits._
      import scalatags.JsDom.svgTags.{svg, path, g, text}
      import scalatags.JsDom.svgAttrs.{d, fill, stroke, width, height, transform, viewBox, xmlns, attr}
      def lineText(str: String, pos: Vec2, height: Double = 20.0) = {
        val xHeight = glyphMap('X').height
        val scale = height / xHeight
        var x = 0.0
        g(
          transform := s"translate(${pos.x},${pos.y}) scale($scale,$scale)",
          g(
            (for (char <- str) yield {
              val glyph = glyphMap.getOrElse(char.toUpper, Glyph("", 8, 0))
              val p = path(transform := s"translate($x, 0)", d := glyph.path, fill := "transparent", stroke := "black")
              x += glyph.width + 4
              p
            })(collection.breakOut): _*
          )
        )
      }
      val sizes = (1 to 10) map (_*2)
      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        attr("x-seed") := s"$seed",
        g(
          (for ((size, i) <- sizes.zipWithIndex) yield {
            lineText(s"size $size Sphinx of black quartz, judge my vow", margins.lower + Vec2(0, sizes.map(_+4).take(i).sum), size)
          })(collection.breakOut): _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
  }
}
