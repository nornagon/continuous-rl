package pcgtest

import kit.{AABB, Segment2, Vec2}
import org.scalajs.dom.html
import snabbdom.VNode
import snabbdom.{dsl => *}

import scala.scalajs.js

class Writing(page: AABB) {
  val margins = page.shrink(100)
  private val patch = snabbdom.snabbdom.init(js.Array(
    snabbdom.attributesModule,
    snabbdom.eventlistenersModule
  ))

  def write(c: Byte, size: Double): Seq[Segment2] = {
    def bit(n: Int): Boolean = (c & (1 << n)) != 0
    // + x
    val bits = Map(
      0 -> Segment2(Vec2(0, 0), Vec2(size/2.0, size/2.0)),
      1 -> Segment2(Vec2(size, 0), Vec2(size/2.0, size/2.0)),
      2 -> Segment2(Vec2(0, size), Vec2(size/2.0, size/2.0)),
      3 -> Segment2(Vec2(size, size), Vec2(size/2.0, size/2.0)),
      4 -> Segment2(Vec2(size/2.0, 0), Vec2(size/2.0, size/2.0)),
      5 -> Segment2(Vec2(0, size/2.0), Vec2(size/2.0, size/2.0)),
      6 -> Segment2(Vec2(size/2.0, size/2.0), Vec2(size/2.0, size/2.0))
    )
    (0 to 7).collect {
      case b if (b == 5 && !bit(b)) || (b != 5 && bit(b)) => Segment2(Vec2(size, size)/2, Vec2.forAngle((b-1)/8.0*Math.PI*2)*size/2.0+Vec2(size, size)/2)
    }
  }

  def writeString(s: String, size: Double): Seq[Seq[Segment2]] = {
    for ((glyph, i) <- s.getBytes.map(write(_, size)).zipWithIndex) yield {
      glyph.map(s => s.translate(Vec2(i * size, 0)))
    }
  }
  val strs = Seq(
    "spring!may—",
    "everywhere's here",
    "(with a low high low",
    "and the bird on the bough)",
    "how?why",
    "—we never we know",
    "(so kiss me) shy sweet eagerly my"
  )

  def render(): VNode = {
    *.svg(
      *.xmlns := "http://www.w3.org/2000/svg",
      *.width := s"${page.width / 100.0}in",
      *.height := s"${page.height / 100.0}in",
      *.viewBox := s"0 0 ${page.width} ${page.height}",
      *.g(
        strs.zipWithIndex.flatMap {
          case (str, lineno) =>
            writeString(str, 20).flatMap(s => s.map(x => *.path(*.d := x.translate(margins.lower + Vec2(0, 20) * lineno).toSVG)))
        }
      )
    )
  }

  def main(root: html.Div): Unit = {
    patch(root, render())
  }
}
