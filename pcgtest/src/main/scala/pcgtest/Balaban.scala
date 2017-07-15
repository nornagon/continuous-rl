package pcgtest

import kit._
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}
import snabbdom.{VNode, dsl => *}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.|

class Balaban(page: AABB, seed: Int) {
  def three(root: html.Div): Unit = {
    val patch = snabbdom.snabbdom.init(js.Array(
      snabbdom.attributesModule,
      snabbdom.eventlistenersModule
    ))

    def render(segs: Seq[Segment2]): VNode = {
      val intersections = Balaban95.intersectingPairs(segs)
      *.svg(
        *.xmlns := "http://www.w3.org/2000/svg",
        *.width := s"${page.width / 100.0}in",
        *.height := s"${page.height / 100.0}in",
        *.viewBox := s"0 0 ${page.width} ${page.height}",
        *.attr("x-seed") := seed.toString,
        *.g(
          segs.map(s => *.path(*.d := s.toSVG))
        ),
        *.g(
          intersections.map {
            case (_, _, Intersections.PointIntersection(p)) =>
              *.circle(*.cx := p.x, *.cy := p.y, *.r := 2)
          }
        )
      )
    }

    var segs = mutable.Buffer.empty[Segment2]

    var vroot: VNode | dom.Element = root
    vroot = patch(vroot, render(segs))

    var lastPos: Option[Vec2] = None
    dom.document.onclick = (e: MouseEvent) => {
      val p = Vec2(e.pageX, e.pageY)
      if (lastPos.isDefined) {
        segs.append(Segment2(lastPos.get, p))
        vroot = patch(vroot, render(segs))
        lastPos = None
      } else {
        lastPos = Some(p)
      }
    }
  }
}
