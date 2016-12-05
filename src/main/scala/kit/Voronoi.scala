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
  case class Beach(site: Vec2, id: Int = { nextBeachId += 1; nextBeachId })
  case class Cell(site: Vec2)
  val cells = mutable.Buffer[Cell]()

  val beaches = mutable.Buffer[Beach]()

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
      beaches.remove(beaches.indexOf(nc.get._1))
    }
    false
  }

  def nextCircle(): Option[(Beach, Circle2)] = {
    val cs = circles()
    if (cs.isEmpty) None
    else Some(cs.minBy { case (_, c) => c.c.y + c.r })
  }

  def circles(): Seq[(Beach, Circle2)] = {
    (for {
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
    }).toSeq
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

  def beachProjections(directrix: Double): Seq[Double] = {
    if (beaches.isEmpty) return Seq.empty
    val breakPoints = beaches.zip(beaches.tail).map { case (a, b) => leftBreakPoint(a.site, b.site, directrix) }
    (Double.NegativeInfinity +: breakPoints :+ Double.PositiveInfinity)(collection.breakOut)
  }

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

  def leftBreakPoint(leftParabola: Vec2, rightParabola: Vec2, directrix: Double): Double = {
    val Vec2(rfocx, rfocy) = rightParabola
    val pby2 = rfocy - directrix
    if (pby2 == 0) return rfocx

    val Vec2(lfocx, lfocy) = leftParabola
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
