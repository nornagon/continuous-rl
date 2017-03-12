package pcgtest

import kit._
import kit.pcg.LloydRelaxation
import org.scalajs.dom
import org.scalajs.dom.html
import kit.CanvasHelpers._

import scala.collection.mutable

class Particles(page: AABB, seed: Int) {

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
    import scalatags.JsDom.svgAttrs.{d, fill, height, stroke, width}
    import scalatags.JsDom.svgTags.{g, path, svg}
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
    val noiseX = new kit.pcg.Noise(4)
    val noiseY = new kit.pcg.Noise(3)
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



  def particles(canvas: html.Canvas): Unit = {
    case class ParticleM(var p: Vec2, var v: Vec2)
    val particles = mutable.Buffer[ParticleM]()
    val screen = AABB(Vec2(0, 0), Vec2(canvas.width, canvas.height))
    //for (i <- 1 to 100) particles += Particle(Rand.withinAABB(screen), Rand.withinCircle())
    particles ++= Circle2(Vec2(canvas.width / 2.0, canvas.height / 2.0), 100).toPolygon(100).points.map(v => ParticleM(v, Vec2(0, 0)))
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val noiseX = new kit.pcg.Noise(0)
    val noiseY = new kit.pcg.Noise(1)
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
}
