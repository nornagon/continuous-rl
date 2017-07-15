package kit

object Intersections {
  sealed trait Intersection
  case class PointIntersection(p: Vec2) extends Intersection
  case class SegmentIntersection(s: Segment2) extends Intersection

  def intersections(a: Circle2, b: Circle2): Iterable[Intersection] = {
    val d2 = (a.c -> b.c).lengthSquared
    val rsum2 = (a.r + b.r) * (a.r + b.r)
    if (d2 > rsum2)
      return Seq.empty
    if (d2 == rsum2)
      return Seq(PointIntersection(a.c.lerp(b.c, math.sqrt(d2)/2)))
    ??? // >1 intersection
  }
  def intersects(a: Circle2, b: Circle2): Boolean = {
    val d2 = (a.c -> b.c).lengthSquared
    val rsum2 = (a.r + b.r) * (a.r + b.r)
    d2 <= rsum2
  }

  def intersections(a: Arc2, b: Arc2): Iterable[Intersection] = ???
  def intersects(a: Arc2, b: Arc2): Boolean = ???

  def intersections(a: Segment2, b: Segment2): Iterable[Intersection] = {
    // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
    val dir = a.a -> a.b
    val otherDir = b.a -> b.b
    val dxo = dir cross otherDir
    if (dxo == 0) {
      if (((a.a -> b.a) cross dir) == 0) {
        // The two segments are collinear.
        // t0 and t1 are the expressions of the endpoints of b in terms of the equation of a.
        val t0 = ((a.a -> b.a) dot dir) / (dir dot dir)
        val t1 = t0 + ((otherDir dot dir) / (dir dot dir))
        if ((t1 > t0 && t0 <= 1 && t1 >= 0) || (t1 <= t0 && t1 <= 1 && t0 >= 0)) {
          // collinear and intersecting, return overlapping part of the segment
          // [t0, t1] overlaps [0, 1]
          Seq(SegmentIntersection(Segment2(a.sample(math.max(t0, 0)), a.sample(math.min(t1, 1)))))
        } else {
          // collinear but not intersecting
          Seq.empty
        }
      } else {
        // The two segments are parallel and non-intersecting.
        Seq.empty
      }
    } else {
      // If two segments are not collinear and meet at a common endpoint, return the common endpoint
      // exactly.
      if (a.a == b.a || a.a == b.b) return Seq(PointIntersection(a.a))
      if (a.b == b.a || a.b == b.b) return Seq(PointIntersection(a.b))
      val t = ((a.a -> b.a) cross otherDir) / dxo
      val u = ((a.a -> b.a) cross dir) / dxo
      if (0 <= t && t <= 1 && 0 <= u && u <= 1)
        Seq(PointIntersection(a.a + dir * t))
      else
        Seq.empty
    }
  }
  def intersects(a: Segment2, b: Segment2): Boolean = intersections(a, b).nonEmpty

  def intersections(a: Polygon, b: Polygon): Iterable[Intersection] = ???
  def intersects(a: Polygon, b: Polygon): Boolean = ???

  def intersections(a: AABB, b: AABB): Iterable[Intersection] = ???
  def intersects(a: AABB, b: AABB): Boolean =
    a.lower.x <= b.upper.x &&
      b.lower.x <= a.upper.x &&
      a.lower.y <= b.upper.y &&
      b.lower.y <= a.upper.y

  @Reversible def intersections(a: Circle2, b: Arc2): Iterable[Intersection] = ???
  @Reversible def intersects(a: Circle2, b: Arc2): Boolean = ???

  @Reversible def intersections(a: Circle2, b: Segment2): Iterable[Intersection] = ???
  @Reversible def intersects(a: Circle2, b: Segment2): Boolean = (b.closestPointTo(a.c) - a.c).length <= a.r

  @Reversible def intersections(a: Circle2, b: Polygon): Iterable[Intersection] = ???
  @Reversible def intersects(a: Circle2, b: Polygon): Boolean = ???

  @Reversible def intersections(a: Circle2, b: AABB): Iterable[Intersection] = ???
  @Reversible def intersects(a: Circle2, b: AABB): Boolean = {
    val Circle2(c, r) = a
    val AABB(lower, upper) = b
    c.x + r >= lower.x && c.x - r <= upper.x && c.y + r >= lower.y && c.y - r <= upper.y
  }

  @Reversible def intersections(a: Arc2, b: Segment2): Iterable[Intersection] = ???
  @Reversible def intersects(a: Arc2, b: Segment2): Boolean = ???

  @Reversible def intersections(a: Arc2, b: Polygon): Iterable[Intersection] = ???
  @Reversible def intersects(a: Arc2, b: Polygon): Boolean = ???

  @Reversible def intersections(a: Arc2, b: AABB): Iterable[Intersection] = ???
  @Reversible def intersects(a: Arc2, b: AABB): Boolean = ???

  @Reversible def intersections(a: Segment2, b: Polygon): Iterable[Intersection] = ???
  @Reversible def intersects(a: Segment2, b: Polygon): Boolean = {
    b.segments.exists(intersects(a, _))
  }

  @Reversible def intersections(seg: Segment2, aabb: AABB): Iterable[Intersection] =
    aabb.segments.flatMap(s => intersections(s, seg))
  @Reversible def intersects(a: Segment2, b: AABB): Boolean = ???

  @Reversible def intersections(a: Polygon, b: AABB): Iterable[Intersection] = ???
  @Reversible def intersects(a: Polygon, b: AABB): Boolean = ???

  def intersections(a: Shape2, b: Shape2): Iterable[Intersection] = a match {
    case a: Circle2 =>
      b match {
        case b: Circle2 => intersections(a, b)
        case b: Arc2 => intersections(a, b)
        case b: Segment2 => intersections(a, b)
        case b: Polygon => intersections(a, b)
        case b: AABB => intersections(a, b)
      }
    case a: Arc2 =>
      b match {
        case b: Circle2 => intersections(a, b)
        case b: Arc2 => intersections(a, b)
        case b: Segment2 => intersections(a, b)
        case b: Polygon => intersections(a, b)
        case b: AABB => intersections(a, b)
      }
    case a: Segment2 =>
      b match {
        case b: Circle2 => intersections(a, b)
        case b: Arc2 => intersections(a, b)
        case b: Segment2 => intersections(a, b)
        case b: Polygon => intersections(a, b)
        case b: AABB => intersections(a, b)
      }
    case a: Polygon =>
      b match {
        case b: Circle2 => intersections(a, b)
        case b: Arc2 => intersections(a, b)
        case b: Segment2 => intersections(a, b)
        case b: Polygon => intersections(a, b)
        case b: AABB => intersections(a, b)
      }
    case a: AABB =>
      b match {
        case b: Circle2 => intersections(a, b)
        case b: Arc2 => intersections(a, b)
        case b: Segment2 => intersections(a, b)
        case b: Polygon => intersections(a, b)
        case b: AABB => intersections(a, b)
      }
  }

  def intersects(a: Shape2, b: Shape2): Boolean = a match {
    case a: Circle2 =>
      b match {
        case b: Circle2 => intersects(a, b)
        case b: Arc2 => intersects(a, b)
        case b: Segment2 => intersects(a, b)
        case b: Polygon => intersects(a, b)
        case b: AABB => intersects(a, b)
      }
    case a: Arc2 =>
      b match {
        case b: Circle2 => intersects(a, b)
        case b: Arc2 => intersects(a, b)
        case b: Segment2 => intersects(a, b)
        case b: Polygon => intersects(a, b)
        case b: AABB => intersects(a, b)
      }
    case a: Segment2 =>
      b match {
        case b: Circle2 => intersects(a, b)
        case b: Arc2 => intersects(a, b)
        case b: Segment2 => intersects(a, b)
        case b: Polygon => intersects(a, b)
        case b: AABB => intersects(a, b)
      }
    case a: Polygon =>
      b match {
        case b: Circle2 => intersects(a, b)
        case b: Arc2 => intersects(a, b)
        case b: Segment2 => intersects(a, b)
        case b: Polygon => intersects(a, b)
        case b: AABB => intersects(a, b)
      }
    case a: AABB =>
      b match {
        case b: Circle2 => intersects(a, b)
        case b: Arc2 => intersects(a, b)
        case b: Segment2 => intersects(a, b)
        case b: Polygon => intersects(a, b)
        case b: AABB => intersects(a, b)
      }
  }
}

object IntersectionsExperiment {
  import Intersections._
  sealed class =!=[A,B]

  trait LowerPriorityImplicits {
    implicit def equal[A]: =!=[A, A] = sys.error("should not be called")
  }
  object =!= extends LowerPriorityImplicits {
    implicit def nequal[A,B](implicit same: A =:= B = null): =!=[A,B] =
      if (same != null) sys.error("should not be called explicitly with same type")
      else new =!=[A,B]
  }

  trait Intersectable[A, B] {
    def intersects(a: A, b: B): Boolean
    def intersections(a: A, b: B): Iterable[Intersection]
  }

  object Intersectable {
    implicit def reverseIntersectable[A, B](implicit ev: A =!= B, i: Intersectable[A, B]) = new Intersectable[B, A] {
      override def intersects(a: B, b: A): Boolean = ???
      override def intersections(a: B, b: A): Iterable[Intersection] = ???
    }
  }

  def isect[A, B](a: A, b: B)(implicit i: Intersectable[A, B]): Boolean = i.intersects(a, b)
  def isect[A <: Shape2, B](a: A, b: B): Boolean = {
    a match {
      case a: Circle2 => isect(a, b)
    }
  }

  implicit val circle2_circle2 = new Intersectable[Circle2, Circle2] {
    def intersects(a: Circle2, b: Circle2): Boolean = ???
    def intersections(a: Circle2, b: Circle2): Iterable[Intersection] = ???
  }

  implicit val circle2_arc2 = new Intersectable[Circle2, Arc2] {
    def intersects(a: Circle2, b: Arc2): Boolean = ???
    def intersections(a: Circle2, b: Arc2): Iterable[Intersection] = ???
  }

  implicit val circle2_segment2 = new Intersectable[Circle2, Segment2] {
    def intersects(a: Circle2, b: Segment2): Boolean = ???
    def intersections(a: Circle2, b: Segment2): Iterable[Intersection] = ???
  }
}
