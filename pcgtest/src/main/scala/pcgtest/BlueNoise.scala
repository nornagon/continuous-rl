package pcgtest

import kit.pcg.PoissonDisk
import kit.{AABB, Tweakable}
import org.scalajs.dom
import org.scalajs.dom.html
import snabbdom.{VNode, dsl => *}

import scala.scalajs.js
import scala.scalajs.js.|

class BlueNoise(page: AABB, seed: Int) {
  private val margins = page.shrink(50)
  private val patch = snabbdom.snabbdom.init(js.Array(
    snabbdom.attributesModule,
    snabbdom.eventlistenersModule
  ))

  @Tweakable.Options
  case class Params(
    @Tweakable.Range(1, 100)
    minDist: Double = 10,
    @Tweakable.Range(1, 100)
    maxPlacementAttempts: Int = 16,
  )


  def render(params: Params): VNode = {
    import params._

    implicit val r = new scala.util.Random(42)

    val points = PoissonDisk.generate(margins, minDist, maxPlacementAttempts = maxPlacementAttempts)

    *.svg(
      *.xmlns := "http://www.w3.org/2000/svg",
      *.width := s"${page.width / 100.0}in",
      *.height := s"${page.height / 100.0}in",
      *.viewBox := s"0 0 ${page.width} ${page.height}",
      *.g(
        points.map(p => *.circle(*.cx := p.x, *.cy := p.y, *.r := 1, *.style := "fill: transparent; stroke: black;"))
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
