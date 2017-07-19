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
    @Tweakable.Range(0, 1) tolerance: Double = 0.5,
    @Tweakable.Range(0, 1000) nTrips: Int = 100,
    showVoronoi: Boolean = false,
  )

  case class Link private (a: Int, b: Int)
  object Link {
    def apply(a: Int, b: Int): Link = Link(math.min(a, b), math.max(a, b))
  }

  def render(params: Params): VNode = {
    import params._
    implicit val r = new scala.util.Random(42)

    val poisNoise = LayeredNoise.octaves(octaves)
    val pois = PoissonDisk.generateModulated(margins, (v: Vec2) => math.max(15, math.min(120, poisNoise.at(v/s) * noisiness + 80)), 120)

    /*
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
    */
    val voronoiLinks: Seq[(Int, Int)] = Voronoi.computeD3Links(pois).filter(_.length <= 200).map {
      case Segment2(a, b) => (pois.indexOf(a), pois.indexOf(b))
    }

    val linkConns = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int, Int]
    val linkWeights = mutable.Map.empty[(Int, Int), Int].withDefault(_ => 0)
    //for (_ <- 1 to nTrips) {
    var nLinks = 0
    while (BFS.reachableFrom(0, (l: Int) => linkConns.getOrElse(l, Set.empty).toSeq).size < pois.size || nLinks < nTrips) {
      nLinks += 1
      val source = r.between(0, pois.size)
      val dest = r.between(0, pois.size)
      if (source != dest) {
        val existingPath = BFS.path[Int](source, l => linkConns.getOrElse(l, Set.empty).toSeq, _ == dest)
        val voronoiPath = BFS.path[Int](source, l => voronoiLinks.collect { case p if p._1 == l => p._2; case p if p._2 == l => p._1 }, _ == dest)
        assert(voronoiPath.nonEmpty)
        if (existingPath.isEmpty) {
          for (Seq(a, b) <- voronoiPath.get.sliding(2)) {
            linkConns.addBinding(a, b)
            linkConns.addBinding(b, a)
            linkWeights((math.min(a, b), math.max(a, b))) += 1
          }
        } else {
          assert(existingPath.isDefined)
          val existingPathLength = existingPath.get.sliding(2).map { case Seq(a, b) => (pois(a) -> pois(b)).length }.sum
          val voronoiPathLength = voronoiPath.get.sliding(2).map { case Seq(a, b) => (pois(a) -> pois(b)).length }.sum
          if (voronoiPathLength < existingPathLength * (1 - tolerance)) {
            for (Seq(a, b) <- voronoiPath.get.sliding(2)) {
              linkConns.addBinding(a, b)
              linkConns.addBinding(b, a)
              linkWeights((math.min(a, b), math.max(a, b))) += 1
            }
          } else {
            for (Seq(a, b) <- existingPath.get.sliding(2)) {
              linkWeights((math.min(a, b), math.max(a, b))) += 1
            }
          }
        }
      }
    }

    val voronoiLinkSegments: Seq[Segment2] = voronoiLinks.map { case (a, b) => Segment2(pois(a), pois(b)) }
    val links = linkConns.flatMap { case (a, bs) => bs.map(b => (math.min(a, b), math.max(a, b)))(collection.breakOut) }(collection.breakOut)
    val linkSegments = links.map { case (a, b) => Segment2(pois(a), pois(b)) }


    *.div(
      *.svg(
        *.xmlns := "http://www.w3.org/2000/svg",
        *.width := s"${page.width / 100.0}in",
        *.height := s"${page.height / 100.0}in",
        *.viewBox := s"0 0 ${page.width} ${page.height}",
        *.g(
          pois.map(p => *.circle(*.cx := p.x, *.cy := p.y, *.r := 1, *.style := "fill: transparent; stroke: black;")),
          if (showVoronoi)
            voronoiLinkSegments.map {
              case Segment2(src, tgt) =>
                *.path(*.d := s"M${src.x},${src.y} L${tgt.x},${tgt.y}", *.style := "stroke: lightgray; stroke-width: 3")
            } else None,
          links.map { l =>
            val src = pois(l._1)
            val tgt = pois(l._2)
            val weight = linkWeights(l) / 10.0
            *.path(*.d := s"M${src.x},${src.y} L${tgt.x},${tgt.y}", *.style := s"stroke-width: ${weight}")
          },
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

