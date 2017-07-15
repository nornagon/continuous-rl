package pcgtest

import kit.pcg.LloydRelaxation
import kit.{AABB, Tweakable, Vec2}
import org.scalajs.dom
import org.scalajs.dom.html
import snabbdom.VNode
import snabbdom.{dsl => *}

import scala.scalajs.js
import scala.scalajs.js.|

class DeJong(page: AABB, seed: Int) {
  private val margins = page.shrink(50)
  private val patch = snabbdom.snabbdom.init(js.Array(
    snabbdom.attributesModule,
    snabbdom.eventlistenersModule
  ))

  def next(a: Double, b: Double, c: Double, d: Double)(p: Vec2): Vec2 =
    Vec2(
      math.sin(a * p.y) - math.cos(b * p.x),
      math.sin(c * p.x) - math.cos(d * p.y)
    )

  def deJong(a: Double, b: Double, c: Double, d: Double, initial: Vec2): Seq[Vec2] =
    Stream.iterate(initial)(next(a, b, c, d))

  @Tweakable.Options
  case class Params(
    @Tweakable.Range(-6,6)
    a: Double = 1.4,
    @Tweakable.Range(-6,6)
    b: Double = -2.3,
    @Tweakable.Range(-6,6)
    c: Double = 2.4,
    @Tweakable.Range(-6,6)
    d: Double = -2.1,
    @Tweakable.Range(1, 6)
    relaxations: Int = 1,
    @Tweakable.Range(100, 10000)
    skipPoints: Int = 0,
    @Tweakable.Range(100, 10000)
    numPoints: Int = 2000
  )

  def render(params: Params): VNode = {
    import params._
    def dot(p: Vec2): VNode = *.circle(*.cx := p.x, *.cy := p.y, *.r := 1)

    val pts = deJong(a, b, c, d, Vec2(0, 0)).map(margins.center + _ * 100).drop(skipPoints).take(numPoints)
    val relaxed = Seq.iterate(pts, relaxations)(LloydRelaxation.voronoiRelax(margins, _).toSeq).last

    *.svg(
      *.xmlns := "http://www.w3.org/2000/svg",
      *.width := s"${page.width / 100.0}in",
      *.height := s"${page.height / 100.0}in",
      *.viewBox := s"0 0 ${page.width} ${page.height}",
      *.g(
        relaxed.filter(margins.contains).map(dot)
      )
    )
  }

  def main(root: html.Div): Unit = {
    var last: VNode | dom.Element = root
    Params.tweakable() { p =>
      last = patch(last, render(p))
    }
  }
}
