package game

import kit._
import org.scalajs.dom
import org.scalajs.dom.{KeyboardEvent, html}
import scala.scalajs.js.annotation.JSExport
import kit.CanvasHelpers._
import kit.pcg.{LloydRelaxation, Noise, SpacePacking}
import kit.cp.Implicits._
import scala.collection.mutable


@JSExport
object PCGTest {
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

  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    //withCanvas(root, spacePacking)
    //withCanvas(root, relaxation)
    //time(root)
    //stars(root)
    //boxes(root)
    //withCanvas(root, noise)
    //withCanvas(root, particles)
    particlesSVG(root)
  }

  def withCanvas(root: html.Div, f: html.Canvas => Unit): Unit = {
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth.toInt
    element.height = dom.window.innerHeight.toInt
    element.style.display = "block"
    root.appendChild(element)
    f(element)
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
  /*
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
  */

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
          for (i <- 0 to s.deadSegments.size) {
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
          }
        }
      }
    }, 1/60.0)
    def stop(): Unit = dom.window.clearInterval(ivl)
    ivl
  }
}
