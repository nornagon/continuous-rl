package kit

import scala.collection.mutable


class Voronoi(sites: Seq[Vec2]) {
  private val lexicographicYX = new Ordering[Vec2] {
    override def compare(a: Vec2, b: Vec2): Int = {
      val dy = math.signum(b.y - a.y).toInt
      if (dy != 0) dy else math.signum(b.x - a.x).toInt
    }
  }

  val sortedSites = sites.sorted(lexicographicYX.reverse)

  var nextBeachId = 0
  case class Beach(
    site: Vec2,
    id: Int = { nextBeachId += 1; nextBeachId }
  )

  val beaches = mutable.Buffer[Beach]()
  val vertices = mutable.Buffer[Vec2]()

  case class Edge(var start: Vec2, var end: Vec2)
  val edges = {
    val underlying = mutable.Map[(Vec2, Vec2), Edge]()
    underlying.withDefault { k =>
      val e = Edge(null, null)
      underlying(k) = e
      e
    }
  }

  val siteEvents = mutable.Buffer[Vec2]() ++ sortedSites
  def nextSite: Option[Vec2] = siteEvents.headOption
  def nextEventY: Option[Double] = {
    val nc = nextCircle()
    val ns = nextSite
    if (nc.isEmpty && ns.isEmpty) return None
    if (ns.isDefined && (nc.isEmpty || nc.get._2.c.y + nc.get._2.r > ns.get.y)) {
      Some(ns.get.y)
    } else Some(nc.get._2.c.y + nc.get._2.r)
  }
  def step(): Boolean = {
    val nc = nextCircle()
    val ns = nextSite
    if (nc.isEmpty && ns.isEmpty) return true
    if (ns.isDefined && (nc.isEmpty || nc.get._2.c.y + nc.get._2.r > ns.get.y)) {
      val site = siteEvents.remove(0)
      addBeach(site)
    } else {
      val Some((beach, circle)) = nc
      val idx = beaches.indexOf(beach)
      val leftBeach = beaches(idx-1)
      val rightBeach = beaches(idx+1)
      val disappearingBeach = beaches.remove(idx)
      val vertex = circle.c
      vertices.append(vertex)
      edges((disappearingBeach.site, leftBeach.site)).start = vertex
      edges((leftBeach.site, disappearingBeach.site)).end = vertex
      edges((rightBeach.site, disappearingBeach.site)).start = vertex
      edges((disappearingBeach.site, rightBeach.site)).end = vertex
      edges((leftBeach.site, rightBeach.site)).start = vertex
      edges((rightBeach.site, leftBeach.site)).end = vertex
    }
    false
  }

  def nextCircle(): Option[(Beach, Circle2)] = {
    val cs = circles()
    if (cs.isEmpty) None
    else Some(cs.minBy { case (_, c) => c.c.y + c.r })
  }

  def circles(): Iterator[(Beach, Circle2)] = {
    for {
      Seq(l, m, r) <- beaches.sliding(3).filter(_.size == 3)
      if l.site != r.site
      if l.site.y > m.site.y || r.site.y > m.site.y
      b = m.site
      a = m.site -> l.site
      c = m.site -> r.site
      d = 2 * (a cross c)
      if d < 0
    } yield {
      val ha = a.lengthSquared
      val hc = c.lengthSquared
      val vec = Vec2(c.y * ha - a.y * hc, a.x * hc - c.x * ha) / d
      (m, Circle2(vec + b, vec.length))
    }
  }

  def addBeach(site: Vec2): Unit = {
    val Vec2(x, directrix) = site
    val beachIdx = findBeachIdxAtX(x, directrix)
    beachIdx match {
      case None =>
        beaches.insert(0, Beach(site))
      case Some(idx) =>
        beaches.insert(idx, Beach(beaches(idx).site), Beach(site))
    }
  }

  /**
    * Returns the list of break points between beaches, starting at NegativeInfinity and ending at PositiveInfinity.
    * @param directrix common directrix
    * @return sequence of break points
    */
  def beachProjections(directrix: Double): Seq[Double] = {
    if (beaches.isEmpty) return Seq.empty
    val breakPoints = beaches.zip(beaches.tail).map { case (a, b) => leftBreakPoint(a.site, b.site, directrix) }
    (Double.NegativeInfinity +: breakPoints :+ Double.PositiveInfinity)(collection.breakOut)
  }

  /**
    * Finds the beach whose parabola has the highest y value at the given x.
    * i.e. find the beach 'under' the point given by (x=x, y=directrix).
    */
  def findBeachIdxAtX(x: Double, directrix: Double): Option[Int] = {
    // TODO: this is slow. use a balanced binary search tree
    def leftEdgeOf(i: Int): Double = {
      if (i == 0) Double.NegativeInfinity
      else leftBreakPoint(beaches(i-1).site, beaches(i).site, directrix)
    }
    def rightEdgeOf(i: Int): Double = {
      if (i == beaches.size - 1) Double.PositiveInfinity
      else leftBreakPoint(beaches(i).site, beaches(i+1).site, directrix)
    }
    beaches.indices.find(i => leftEdgeOf(i) < x && x <= rightEdgeOf(i) )
  }

  /**
    * Finds the intersection of two parabolae with foci at `leftFocus` and `rightFocus` and a common directrix.
    *
    * The two foci are presumed to have y <= directrix. If there are two intersection points (the usual case), the one
    * returned will be the intersection which has the parabola focused at `leftFocus` having greater Y value to the left
    * of the intersection point. That is, given the following two parabolae A and B:
    *
    *    B|    /
    *     |   /|Q
    * A\  |  / |
    *   \  \/_/
    *    \_/P
    *
    * The point P would be returned for `leftBreakPoint(A, B)`, while the point Q would be returned for
    * `leftBreakPoint(B, A)`.
    *
    * Cribbed from d3.js: https://github.com/d3/d3-voronoi/blob/a08f96556af848ffe5fcbbc89d1b296abef34986/src/Beach.js#L161
    *
    * @param leftFocus the focus of the parabola on the left of the desired intersection point
    * @param rightFocus the focus of the parabola on the right of the desired intersection point
    * @param directrix the common directrix
    * @return the 'break point' between `leftFocus` on the left and `rightFocus` on the right
    */
  def leftBreakPoint(leftFocus: Vec2, rightFocus: Vec2, directrix: Double): Double = {
    val Vec2(rfocx, rfocy) = rightFocus
    val pby2 = rfocy - directrix
    if (pby2 == 0) return rfocx

    val Vec2(lfocx, lfocy) = leftFocus
    val plby2 = lfocy - directrix
    if (plby2 == 0) return lfocx

    val hl = lfocx - rfocx
    val aby2 = 1 / pby2 - 1 / plby2
    val b = hl / plby2
    if (aby2 != 0)
      (-b + math.sqrt(b * b - 2 * aby2 * (hl * hl / (-2 * plby2) - lfocy + plby2 / 2 + rfocy - pby2 / 2))) / aby2 + rfocx
    else
      (rfocx + lfocx) / 2
  }
}
