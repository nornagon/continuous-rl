package pcgtest

import kit.RandomImplicits._
import kit._
import org.scalajs.dom
import org.scalajs.dom.html
import snabbdom.VNode
import snabbdom.{dsl => *}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.|
import scala.scalajs.js.JSConverters._

@JSName("ClipperLib.IntPoint")
@js.native
class IntPoint(val X: Double, val Y: Double) extends js.Object {
}

@JSName("ClipperLib.Clipper")
@js.native
class Clipper extends js.Object {
  def AddPaths(path: js.Array[js.Array[IntPoint]], polyType: Int, closed: Boolean): Unit = js.native
  def AddPath(path: js.Array[IntPoint], polyType: Int, closed: Boolean): Unit = js.native
  def Execute(clipType: Int, solution: js.Any, subjFillType: Int, clipFillType: Int): Unit = js.native
  def Execute(clipType: Int, solution: js.Any): Unit = js.native
}

class RoomSpring(page: AABB, seed: Int) {
  private val margins = page.shrink(50)
  private val patch = snabbdom.snabbdom.init(js.Array(
    snabbdom.attributesModule,
    snabbdom.eventlistenersModule
  ))

  private val mulFact = 100000.0

  def polyToPath(p: Polygon): js.Array[IntPoint] =
    p.points.map(p => new IntPoint(p.x * mulFact, p.y * mulFact)).toJSArray
  def pathToPoly(ps: js.Array[IntPoint]): Polygon =
    Polygon(ps map { p => Vec2(p.X / mulFact, p.Y / mulFact) })

  def subtract(a: Polygon, bs: Polygon*): Seq[Polygon] = {
    val clipper = new Clipper
    clipper.AddPath(polyToPath(a), 0, true)
    for (b <- bs)
      clipper.AddPath(polyToPath(b), 1, true)
    val solution = js.Array[js.Array[IntPoint]]()
    clipper.Execute(2, solution,0 , 2)
    for (points <- mutable.Seq() ++ solution) yield pathToPoly(points)
  }

  @Tweakable.Options
  case class Params(
    @Tweakable.Range(0, 1000)
    time: Double = 0,
    doSubtract: Boolean = true
  )

  def forces(rooms: Seq[AABB], springs: Seq[(Int, Int, Double)]): Seq[Vec2] = {
    val forces = mutable.Seq.fill(rooms.size)(Vec2(0, 0))
    // Compute the force on each room according to the springs
    for ((a, b, k) <- springs) {
      val aCenter = rooms(a).center
      val bCenter = rooms(b).center
      val dir = aCenter -> bCenter
      val force = dir * -k
      forces(a) -= force
      forces(b) += force
    }
    // Then add separation force proportional to the area of overlap
    for (a <- rooms.indices; b <- a + 1 until rooms.size) {
      val aRoom = rooms(a)
      val bRoom = rooms(b)
      val overlap = math.sqrt(aRoom overlapArea bRoom)
      if (overlap > 0) {
        val dir = (aRoom.center -> bRoom.center).normed
        forces(a) -= dir * overlap
        forces(b) -= dir * -overlap
      }
    }
    for (a <- rooms.indices; b <- a + 1 until rooms.size) {
      val aRoom = rooms(a)
      val bRoom = rooms(b)
      val overlap = math.sqrt(aRoom.expand(1) overlapArea bRoom.expand(1))

      if (overlap > 0) {
        // For rooms that overlap, find their closest edges and try to make them align
        val (aVerts, aHorizs) = aRoom.toPolygon.segments.partition(e => e.a.x == e.b.x)
        val (bVerts, bHorizs) = bRoom.toPolygon.segments.partition(e => e.a.x == e.b.x)
        val pairs = (for (aV <- aVerts; bV <- bVerts) yield (aV, bV)) ++ (for (aH <- aHorizs; bH <- bHorizs) yield (aH, bH))
        val closestEdge = pairs.map {
          case (edgeA, edgeB) =>
            if (edgeA.a.x == edgeA.b.x) {
              // vertical, use the x distance
              Vec2(edgeA.a.x - edgeB.a.x, 0)
            } else {
              // horizontal, use the y distance
              Vec2(0, edgeA.a.y - edgeB.a.y)
            }
        }.minBy(_.lengthSquared)
        forces(a) -= closestEdge.normed * 0.2
        forces(b) += closestEdge.normed * 0.2
      }
    }

    for (i <- forces.indices) {
      val maxedForce = forces(i).normed * math.min(100, forces(i).length)
      forces(i) = maxedForce
    }
    forces
  }

  def step(rooms: Seq[AABB], springs: Seq[(Int, Int, Double)], dt: Double): Seq[AABB] = {
    // Integrate
    val fs = forces(rooms, springs)
    rooms.zipWithIndex map {
      case (r, i) =>
        r.translate(fs(i) * dt)
    }
  }

  def randomRooms()(implicit r: scala.util.Random): (Seq[AABB], Seq[(Int, Int, Double)]) = {
    val rooms = (1 to r.between(5, 8)).map { _ =>
      val width = r.between(3, 8)
      val height = r.between(3, 8)
      AABB(-width/2, -height/2, width/2, height/2).translate(r.withinCircle(5))
    }
    val springs = (1 to rooms.size) map { _ =>
      val a = r.between(0, rooms.size)
      val b = r.between(0, rooms.size)
      (a, b, r.between(0.2, 0.4))
    }
    (rooms, springs)
  }

  def render(params: Params): VNode = {
    import params._
    val dt = 0.1

    println(subtract(AABB(0, 0, 10, 10).toPolygon, AABB(5, 5, 10, 10).toPolygon))

    implicit val r = new scala.util.Random(seed)

    val (rooms, springs) = randomRooms()
    val evolved = Stream.iterate((rooms, 0)) { case (rs, n) => (step(rs, springs, dt * (1/(n*0.0+1.0))), n + 1) }
    val evolvedRooms = evolved.drop(time.round.toInt).head._1
    val fs = forces(evolvedRooms, springs)

    val clippedRooms = if (doSubtract) {
      for (i <- evolvedRooms.indices) yield {
        val aboveMe = evolvedRooms.drop(i+1)
        if (aboveMe.isEmpty)
          Seq(evolvedRooms(i).toPolygon)
        else
          subtract(evolvedRooms(i).toPolygon, aboveMe.map(_.toPolygon): _*)
      }
    } else {
      Seq(evolvedRooms.map(_.toPolygon))
    }

    *.svg(
      *.xmlns := "http://www.w3.org/2000/svg",
      *.width := s"${page.width / 100.0}in",
      *.height := s"${page.height / 100.0}in",
      *.viewBox := s"0 0 ${page.width} ${page.height}",
      *.g(
        *.transform := s"translate(${margins.center.x}, ${margins.center.y}) scale(20)",
        clippedRooms.flatten.map(r => *.path(*.d := r.toSVG, *.vectorEffect := "non-scaling-stroke")),
        fs.zip(evolvedRooms).map { case (f, r) => *.path(*.d := Segment2(r.center, r.center + f*dt).toSVG, *.vectorEffect := "non-scaling-stroke") }
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
