package kit

import scala.collection.mutable


class Voronoi(sites: Seq[Vec2]) {
  private val lexicographicYX = new Ordering[Vec2] {
    override def compare(a: Vec2, b: Vec2): Int = {
      val dy = math.signum(b.y - a.y).toInt
      if (dy != 0) dy else math.signum(b.x - a.x).toInt
    }
  }
  val sortedSites = mutable.Queue() ++ sites.sorted(lexicographicYX)
  case class Beach()
  case class Circle(x: Double, y: Double)
  //val beaches = mutable.TreeSet.empty[Beach]
  val circles = mutable.TreeSet.empty[Circle](Ordering.by(_.y))

  def addBeach(v: Vec2): Unit = ???
  def removeBeach(beach: Beach): Unit = ???

  /** Given two parabola with foci at `f1` and `f2` and a common horizontal directrix, return the x value of their intersection.
    * Cribbed from d3-voronoi, I haven't checked this math myself. */
  def parabolaIntersection(directrix: Double, f1: Vec2, f2: Vec2): Double = {
    val (lfoc, rfoc) = if (f1.x < f2.x) (f1, f2) else (f2, f1)
    val pby2 = rfoc.y - directrix
    if (pby2 == 0) return rfoc.x
    val plby2 = lfoc.y - directrix
    if (plby2 == 0) return lfoc.x
    val hl = lfoc.x - rfoc.x
    val aby2 = 1 / pby2 - 1 / plby2
    val b = hl / plby2
    if (aby2 != 0)
      (-b + Math.sqrt(b * b - 2 * aby2 * (hl * hl / (-2 * plby2) - lfoc.y + plby2 / 2 + rfoc.y - pby2 / 2))) / aby2 + rfoc.x
    else
      (rfoc.x + lfoc.x) / 2
  }

  def step(): Unit = {
    // 1. grab the first event, which is either:
    //   a. a point from `sites`, or
    //   b. the edge of a circle formed by 3 adjacent open arcs
    val firstCircle = circles.headOption
    val firstSite = sortedSites.headOption
    if (firstCircle.isEmpty && firstSite.isEmpty)
      return /* done! */
    if (firstSite.isDefined && (firstCircle.isEmpty || firstSite.get.y < firstCircle.get.y || (firstCircle.get.y == firstCircle.get.y && firstSite.get.x < firstCircle.get.x))) {
      // hit a site
      addBeach(sortedSites.dequeue())
    } else if (firstCircle.isDefined) {
      //removeBeach(firstCircle.get)
    }
    // 2. if it's a site, add a beach here by splitting the arcs under it
    // 3. if it's a circle, remove the beach associated with the circle
  }
}
