package pcgtest

import kit.pcg.LayeredNoise
import kit.RandomImplicits._
import kit._
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.CanvasRenderingContext2D
import snabbdom.{VNode, dsl => *}
import kit.CanvasHelpers._

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.|

class Tectonics(page: AABB, seed: Int) {
  private val margins = page.shrink(50)
  private val patch = snabbdom.snabbdom.init(js.Array(
    snabbdom.attributesModule,
    snabbdom.eventlistenersModule
  ))

  @Tweakable.Options
  case class Params(
    scale: Double = 256.0,
    @Tweakable.Range(0, 10)
    time: Double = 0,
    force: Double = 2000,
    springStrength: Double = -9,
    maxLength: Double = 60,
    restLength: Double = 20,
    displayMagma: Boolean = false,
  )

  def render(params: Params): VNode = {
    import params._
    val timeDiv = 50

    implicit val r = new scala.util.Random(43)

    val vectionX = new kit.pcg.Noise(r.nextInt())
    val vectionY = new kit.pcg.Noise(r.nextInt())

    var particles: Seq[Vec2] = (1 to 1000) map { _ => r.withinAABB(page) }
    val springs = Voronoi.computeD3Links(particles).filter(_.length < 60).map { case Segment2(a, b) => particles.indexOf(a) -> particles.indexOf(b) }
    val springsByIndex: Map[Int, Seq[Int]] = particles.indices.map({ i =>
      i -> springs.collect { case (a, b) if a == i => b; case (a, b) if b == i => a }
    })(collection.breakOut)
    def pForce(p: Vec2, t: Double): Vec2 = Vec2(vectionX.simplex3(p.x/scale, p.y/scale, t), vectionY.simplex3(p.x/scale, p.y/scale, t)) * force
    val dt = 0.1 / timeDiv
    for (t <- 0d until (time/timeDiv) by dt) {
      particles = for (i <- particles.indices) yield {
        val p = particles(i)
        val ss = springsByIndex(i) map particles
        val k = springStrength
        val sf = (Vec2(0, 0) /: ss) { (a, s) =>
          val vec = p -> s
          a + (if (vec.lengthSquared > maxLength*maxLength) Vec2(0, 0) else vec.normed * (vec.length - restLength) * -k)
        }
        val f = pForce(p, t) + sf
        p + f * dt
      }
    }

    *.div(
      *.renderCanvas(*.width := page.width, *.height := page.height) { (ctx: CanvasRenderingContext2D) =>
        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
        if (displayMagma) {
          val data = ctx.createImageData(ctx.canvas.width, ctx.canvas.height)
          for (y <- 0 until ctx.canvas.height; x <- 0 until ctx.canvas.width) {
            val dx = vectionX.simplex3(x / scale, y / scale, time / timeDiv)
            val dy = vectionY.simplex3(x / scale, y / scale, time / timeDiv)
            val xColor = math.floor(128 + 128 * dx).toInt
            val yColor = math.floor(128 + 128 * dy).toInt
            data.data((y * ctx.canvas.width + x) * 4) = 0
            data.data((y * ctx.canvas.width + x) * 4 + 1) = if (math.abs(dx) < 0.05) 255 else xColor
            data.data((y * ctx.canvas.width + x) * 4 + 2) = if (math.abs(dy) < 0.05) 255 else yColor
            data.data((y * ctx.canvas.width + x) * 4 + 3) = 255
          }
          ctx.putImageData(data, 0, 0)
        }
        ctx.fillStyle = "red"
        for (p <- particles) {
          ctx.fillRect(p.x - 1, p.y - 1, 2, 2)
        }
        ctx.strokeStyle = "red"
        ctx.lineWidth = 0.5
        for ((a, b) <- springs; if (particles(a) -> particles(b)).length < maxLength) {
          ctx.beginPath()
          ctx.moveTo(particles(a))
          ctx.lineTo(particles(b))
          ctx.stroke()
        }
      }
    )
  }

  def main(root: html.Div): Unit = {
    var last: VNode | dom.Element = root
    Params.tweakable() { p =>
      last = patch(last, render(p))
    }
  }
}
