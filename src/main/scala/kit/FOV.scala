package kit

import scala.collection.mutable

object FOV {
  case class Endpoint(p: Vec2, angle: Double, segment: Segment2, isBegin: Boolean)

  /** True if `point` is "to the left" of the line collinear with `segment`.
    *        b
    * left  /   x
    *      /
    *     a   right
    */
  def isLeftOf(segment: Segment2, point: Vec2): Boolean =
    ((segment.a -> segment.b) cross (segment.a -> point)) < 0

  /** True if `b` is "closer to" `relativeTo` than `a`.
    * See http://www.redblobgames.com/articles/visibility/segment-sorting.html
    */
  def segmentInFrontOf(a: Segment2, b: Segment2, relativeTo: Vec2): Boolean = {
    val A1 = isLeftOf(a, b.a.lerp(b.b, 0.01))
    val A2 = isLeftOf(a, b.b.lerp(b.a, 0.01))
    val A3 = isLeftOf(a, relativeTo)
    val B1 = isLeftOf(b, a.a.lerp(a.b, 0.01))
    val B2 = isLeftOf(b, a.b.lerp(a.a, 0.01))
    val B3 = isLeftOf(b, relativeTo)

    (B1 == B2 && B2 != B3) || (A1 == A2 && A2 == A3)
  }

  def lineIntersection(s1: Segment2, s2: Segment2): Vec2 = {
    val s2v = s2.a -> s2.b
    val s = (s2v cross (s1.a - s2.a)) / ((s1.b - s1.a) cross s2v)
    s1.a + (s1.a -> s1.b) * s
  }

  def getTrianglePoints(source: Vec2, a1: Double, a2: Double, segment: Option[Segment2]): Seq[Vec2] = {
    val v1 = Vec2.forAngle(a1)
    val v2 = Vec2.forAngle(a2)
    val seg = segment match {
      case Some(s: Segment2) => s
      case None => Segment2(source + v1 * 2000, source + v2 * 2000)
    }

    Seq(
      lineIntersection(seg, Segment2(source, source + v1)),
      lineIntersection(seg, Segment2(source, source + v2))
    )
  }

  /** Amit Patel's algorithm for computing field of view.
    *
    * @see http://www.redblobgames.com/articles/visibility/
    * @param source The source of the "vision"
    * @param segments A list of segments which block vision.
    * @param bounds A bounding box beyond which vision will not propagate.
    * @return A list of vertices which form the edge of vision.
    */
  def calculateFOV(source: Vec2, segments: Seq[Segment2], bounds: AABB): Seq[Vec2] = {
    val endpoints = (segments ++ bounds.toPolygon.segments).flatMap { segment =>
      bounds.truncate(segment) match {
        case None =>
          Seq.empty
        case Some(truncated) =>
          val aAngle = (source -> segment.a).toAngle
          val bAngle = (source -> segment.b).toAngle
          var dAngle = bAngle - aAngle
          if (dAngle <= -Math.PI) dAngle += 2 * Math.PI
          if (dAngle > Math.PI) dAngle -= 2 * Math.PI
          Seq(
            Endpoint(segment.a, aAngle, truncated, dAngle > 0),
            Endpoint(segment.b, bAngle, truncated, dAngle <= 0)
          )
      }
    }.sortWith((a, b) => a.angle < b.angle || (a.angle == b.angle && a.isBegin && !b.isBegin))

    // TODO: This data structure could definitely be more efficient. SortedSet doesn't work unfortunately because
    // segmentInFrontOf is not a total ordering.
    var openSegments = List.empty[Segment2]
    def addSeg(seg: Segment2): Unit = {
      val (closer, further) = openSegments.span(s => segmentInFrontOf(seg, s, source))
      openSegments = closer ++ (seg :: further)
    }
    def delSeg(seg: Segment2): Unit = {
      val (before, after) = openSegments.span(_ ne seg)
      openSegments = before ++ after.drop(1)
    }
    var beginAngle: Double = 0
    // The first pass is just to set up `beginAngle` and `openSegments` correctly. TODO: do this more efficiently.
    for (endpoint <- endpoints) {
      val previouslyClosestSegment = openSegments.headOption
      if (endpoint.isBegin)
        addSeg(endpoint.segment)
      else
        delSeg(endpoint.segment)
      if (previouslyClosestSegment != openSegments.headOption)
        beginAngle = endpoint.angle
    }
    val output = mutable.Buffer[Seq[Vec2]]()
    for (endpoint <- endpoints) {
      val previouslyClosestSegment = openSegments.headOption
      if (endpoint.isBegin)
        addSeg(endpoint.segment)
      else
        delSeg(endpoint.segment)
      if (previouslyClosestSegment != openSegments.headOption) {
        output.append(getTrianglePoints(source, beginAngle, endpoint.angle, previouslyClosestSegment))
        beginAngle = endpoint.angle
      }
    }
    output.flatten
  }
}
