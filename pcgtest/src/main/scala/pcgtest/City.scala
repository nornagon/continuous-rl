package pcgtest

import kit.pcg.{LayeredNoise, PoissonDisk}
import kit._
import kit.RandomImplicits._
import org.scalajs.dom
import org.scalajs.dom.html
import snabbdom.{VNode, dsl => *}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.|

class City(page: AABB, seed: Int) {
  private val margins = page.shrink(50)
  private val patch = snabbdom.snabbdom.init(js.Array(
    snabbdom.attributesModule,
    snabbdom.eventlistenersModule
  ))

  @Tweakable.Options
  case class Params(
    @Tweakable.Range(1, 400) s: Double = 120,
    @Tweakable.Range(1, 10) octaves: Int = 2,
    @Tweakable.Range(20, 200) noisiness: Double = 100,
    @Tweakable.Range(0, 80) nExtra: Int = 20,
  )

  def render(params: Params): VNode = {
    import params._
    implicit val r = new scala.util.Random(42)

    val poisNoise = LayeredNoise.octaves(octaves)
    val pois = PoissonDisk.generateModulated(margins, (v: Vec2) => math.max(15, math.min(120, poisNoise.at(v/s) * noisiness + 80)), 120)

    val links = mutable.Buffer.empty[(Int, Int)]
    def canPlaceLink(newLink: (Int, Int)): Boolean = {
      if (links.contains(newLink) || links.contains((newLink._2, newLink._1)))
        return false
      val newLinkSeg = Segment2(pois(newLink._1), pois(newLink._2))
      !links.exists { existingLink =>
        !((existingLink._1 == newLink._1) || (existingLink._1 == newLink._2) || (existingLink._2 == newLink._1) || (existingLink._2 == newLink._2)) &&
          Intersections.intersects(newLinkSeg, Segment2(pois(existingLink._1), pois(existingLink._2)))
      }
    }
    for (i <- 1 until pois.size) {
      val possibleLinks = Stream.continually((i, r.between(0, i))).take(32)
      val link = possibleLinks
        .filter(canPlaceLink)
        .sortBy { case (a, b) => (pois(a) -> pois(b)).lengthSquared }
        .headOption
      link foreach (l => links.append(l))
    }

    for (_ <- 1 to nExtra) {
      val possibleLinks = Stream.continually((r.between(0, pois.size), r.between(0, pois.size))).take(32)
      val link = possibleLinks
        .filter(l => l._1 != l._2)
        .filter(l => !links.contains(l) && !links.contains((l._2, l._1)))
        .filter(canPlaceLink)
        .sortBy { case (a, b) => (pois(a) -> pois(b)).lengthSquared }
        .headOption
      link foreach (l => links.append(l))
    }

    val linkSegments = links map { case (a, b) => Segment2(pois(a), pois(b)) }

    *.div(
      *.svg(
        *.xmlns := "http://www.w3.org/2000/svg",
        *.width := s"${page.width / 100.0}in",
        *.height := s"${page.height / 100.0}in",
        *.viewBox := s"0 0 ${page.width} ${page.height}",
        *.g(
          pois.map(p => *.circle(*.cx := p.x, *.cy := p.y, *.r := 1, *.style := "fill: transparent; stroke: black;")),
          linkSegments.map {
            case Segment2(src, tgt) =>
              *.path(*.d := s"M${src.x},${src.y} L${tgt.x},${tgt.y}")
          }
        )
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

