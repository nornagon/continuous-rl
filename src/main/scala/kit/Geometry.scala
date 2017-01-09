package kit

import scala.scalajs.js


object Angle {
  /** Returns an angle equivalent to `a` but within the range [-π, π] */
  def clipToPi(a: Double): Double = {
    if (a < -Math.PI)
      a + (Math.PI * 2 * ((a + Math.PI) / (Math.PI * 2)).floor.abs)
    else if (a > Math.PI)
      a - (Math.PI * 2 * ((a - Math.PI) / (Math.PI * 2)).ceil.abs)
    else
      a
  }
}

case class Vec2(x: Double, y: Double) {
  def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
  def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
  def *(k: Double): Vec2 = Vec2(x * k, y * k)
  def /(k: Double): Vec2 = Vec2(x / k, y / k)
  def unary_-(): Vec2 = Vec2(-x, -y)
  /** Hadamard product */
  def o(other: Vec2): Vec2 = Vec2(x * other.x, y * other.y)

  def dot(other: Vec2): Double = x * other.x + y * other.y
  def cross(other: Vec2): Double = x * other.y - other.x * y

  def ->(other: Vec2): Vec2 = other - this

  def lengthSquared = x * x + y * y
  def length = Math.sqrt(lengthSquared)
  def normed = if (length == 0) Vec2(0, 0) else this / length

  def toAngle = Math.atan2(y, x)

  def perp = Vec2(-y, x)

  def lerp(other: Vec2, t: Double): Vec2 = this * (1 - t) + other * t

  def rotate(angle: Double): Vec2 = Mat33.rotate(angle) * this

  override def toString: String = f"$productPrefix%s($x%.2f,$y%.2f)"
}

object Vec2 {
  def forAngle(t: Double) = Vec2(Math.cos(t), Math.sin(t))
  def aroundCircle(numPoints: Int, startAngle: Double = 0): Seq[Vec2] =
    for (i <- 0 until numPoints) yield Vec2.forAngle(i.toDouble / numPoints * 2 * Math.PI + startAngle)
}

case class Vec3(x: Double, y: Double, z: Double) {
  def +(other: Vec3): Vec3 = Vec3(x + other.x, y + other.y, z + other.z)
  def -(other: Vec3): Vec3 = Vec3(x - other.x, y - other.y, z - other.z)
  def *(k: Double): Vec3 = Vec3(x * k, y * k, z * k)
  def /(k: Double): Vec3 = Vec3(x / k, y / k, z * k)

  def dot(other: Vec3): Double = x * other.x + y * other.y + z * other.z

  def ->(other: Vec3): Vec3 = other - this

  def lengthSquared = x * x + y * y + z * z
  def length = Math.sqrt(lengthSquared)
  def normed = if (length == 0) Vec2(0, 0) else this / length
}


/** 2x2 Matrix
  * ( a  b )
  * ( c  d )
  */
case class Mat22(a: Double, b: Double, c: Double, d: Double) {
  def *(v: Vec2): Vec2 = Vec2(a * v.x + b * v.y, c * v.x + d * v.y)
  def *(m: Mat22): Mat22 = Mat22(
    a * m.a + b * m.c, a * m.b + b * m.d,
    c * m.a + d * m.c, c * m.b + d * m.d
  )

  def determinant: Double = a * d - b * c
}

/** 3x3 Matrix
  * ( a  b  c )
  * ( d  e  f )
  * ( g  h  i )
  */
case class Mat33(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double, g: Double, h: Double, i: Double) {
  def *(v: Vec3): Vec3 = Vec3(
    a * v.x + b * v.y + c * v.z,
    d * v.x + e * v.y + f * v.z,
    g * v.x + h * v.y + i * v.z
  )
  def *(m: Mat33): Mat33 = Mat33(
    a * m.a + b * m.d + c * m.g, a * m.b + b * m.e + c * m.h, a * m.c + b * m.f + c * m.i,
    d * m.a + e * m.d + f * m.g, d * m.b + e * m.e + f * m.h, d * m.c + e * m.f + f * m.i,
    g * m.a + h * m.d + i * m.g, g * m.b + h * m.e + i * m.h, g * m.c + h * m.f + i * m.i
  )
  def *(k: Double): Mat33 = Mat33(
    a * k, b * k, c * k,
    d * k, e * k, f * k,
    g * k, h * k, i * k
  )

  def *(v: Vec2): Vec2 = this * Vec3(v.x, v.y, 1) match { case Vec3(x, y, _) => Vec2(x, y) }

  def inverse = {
    val ai = e * i - f * h
    val bi = -(d * i - f * g)
    val ci = d * h - e * g
    val di = -(b * i - c * h)
    val ei = a * i - c * g
    val fi = -(a * h - b * g)
    val gi = b * f - c * e
    val hi = -(a * f - c * d)
    val ii = a * e - b * d
    val det = a * ai + b * bi + c * ci
    if (det == 0)
      throw new RuntimeException(s"Singular matrix can't be inverted: $this")
    Mat33(
      ai, di, gi,
      bi, ei, hi,
      ci, fi, ii
    ) * (1 / det)
  }

  def determinant = {
    val ai = e * i - f * h
    val bi = -(d * i - f * g)
    val ci = d * h - e * g
    a * ai + b * bi + c * ci
  }

  def toSeq: Seq[Double] = Seq(a, b, c, d, e, f, g, h, i)
}
object Mat33 {
  def identity: Mat33 = Mat33(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  )
  def translate(tx: Double, ty: Double): Mat33 = Mat33(
    1, 0, tx,
    0, 1, ty,
    0, 0, 1
  )
  def translate(v: Vec2): Mat33 = translate(v.x, v.y)
  def rotate(theta: Double): Mat33 = {
    val c = Math.cos(theta)
    val s = Math.sin(theta)
    Mat33(
      c, s, 0,
      -s, c, 0,
      0, 0, 1
    )
  }
  def scale(k: Double): Mat33 = {
    Mat33(
      k, 0, 0,
      0, k, 0,
      0, 0, 1
    )
  }
}

trait Shape2 {
  def intersects(other: Shape2): Boolean
}

/** A circle of radius `r` centered at `c`. */
case class Circle2(c: Vec2, r: Double) extends Shape2 {
  def boundingBox = AABB(c - Vec2(r, r), c + Vec2(r, r))

  override def intersects(other: Shape2): Boolean = other match {
    case other: Circle2 => (c - other.c).length <= r + other.r
    case o => o intersects this
  }

  def contains(p: Vec2): Boolean = (c -> p).lengthSquared <= r*r

  def toPolygon(numPoints: Int, startAngle: Double = 0): Polygon =
    Polygon(Vec2.aroundCircle(numPoints, startAngle).map(_ * r)).translate(c)

  def area: Double = Math.PI * r * r
}

case class Arc2(c: Vec2, rx: Double, ry: Double, rotation: Double, startAngle: Double, sweptAngle: Double) {
  assert(rx == ry)
  assert(rotation == 0)
  // https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
  def toSVGArc: SVGArc = {
    val rotate = Mat33.rotate(-rotation)
    val r = Vec2(rx, ry)
    val v0 = rotate * (r o Vec2.forAngle(startAngle)) + c
    val v1 = rotate * (r o Vec2.forAngle(startAngle + sweptAngle)) + c
    SVGArc(v0, v1, rx, ry, rotation, sweptAngle.abs > Math.PI, sweptAngle > 0)
  }

  def translate(p: Vec2): Arc2 = copy(c = c + p)
  def rotateAboutOrigin(angle: Double): Arc2 = {
    if (c == Vec2(0, 0))
      copy(startAngle = startAngle + angle)
    else
      toSVGArc.rotateAboutOrigin(angle).toArc2
  }
  def scale(k: Double) = copy(c = c * k, rx = rx * k, ry = ry * k)

  /** t in [0, 1] */
  def sample(t: Double): Vec2 = Mat33.rotate(rotation) * (Vec2(rx, ry) o Vec2.forAngle(startAngle + sweptAngle * t)) + c
  def toPoints(numPoints: Int): Seq[Vec2] = (0 until numPoints) map { i => sample(i/(numPoints - 1).toDouble) }

  def toSVG: String = toSVGArc.toSVG
  /*def toSVG: String = {
    val r = Vec2(rx, ry)
    val start = c + (r o Vec2.forAngle(startAngle))
    val end = c + (r o Vec2.forAngle(startAngle + sweptAngle))
    s"M${start.x} ${start.y} A $rx $ry 0 0 1 ${end.x} ${end.y}"
  }*/
}

case class SVGArc(start: Vec2, end: Vec2, rx: Double, ry: Double, xRot: Double, largeArc: Boolean, sweep: Boolean) {
  def translate(p: Vec2): SVGArc = copy(start = start + p, end = end + p)
  def rotateAbout(p: Vec2, angle: Double): SVGArc = translate(-p).rotateAboutOrigin(angle).translate(p)
  def rotateAboutOrigin(angle: Double): SVGArc = {
    val mat = Mat33.rotate(angle)
    copy(start = mat * start, end = mat * end)
  }
  def scale(k: Double) = copy(start = start * k, end = end * k, rx = rx * k, ry = ry * k)

  // https://svgwg.org/svg2-draft/implnote.html#ArcImplementationNotes
  def toArc2: Arc2 = {
    val v1p = Mat33.rotate(xRot) * ((start - end) / 2)
    val sgn = if (largeArc != sweep) 1 else -1
    //val radiiCheck = Math.pow(v1p.x, 2)/Math.pow(this.rx, 2) + Math.pow(v1p.y, 2)/Math.pow(this.ry, 2)
    //val rx = if (radiiCheck > 1) math.sqrt(radiiCheck) * this.rx else this.rx
    //val ry = if (radiiCheck > 1) math.sqrt(radiiCheck) * this.ry else this.ry
    val radicand = (rx*rx * ry*ry - rx*rx * v1p.y*v1p.y - ry*ry * v1p.x*v1p.x) / (rx*rx * v1p.y*v1p.y + ry*ry * v1p.x*v1p.x)
    val cp = Vec2(
      rx*v1p.y / ry,
      -ry*v1p.x / rx
    ) * (math.sqrt(radicand max 0) * sgn)
    val c = Mat33.rotate(-xRot) * cp + (start + end) / 2
    def angleBetween(u: Vec2, v: Vec2) = { v.toAngle - u.toAngle }
    val recipRadius = Vec2(1 / rx, 1 / ry)
    val v = (v1p - cp) o recipRadius
    val startAngle = angleBetween(Vec2(1, 0), v)
    val dTheta = angleBetween(v, (-v1p - cp) o recipRadius)
    val sweptAngle = (
      if (!sweep && dTheta > 0) dTheta - 2 * Math.PI
      else if (sweep && dTheta < 0) dTheta + 2 * Math.PI
      else dTheta
    ) % (2 * Math.PI)
    if (c.x.isNaN) js.debugger()
    if (c.y.isNaN) js.debugger()
    if (startAngle.isNaN) js.debugger()
    if (sweptAngle.isNaN) js.debugger()
    Arc2(c, rx, ry, xRot, startAngle, sweptAngle)
  }

  def toSVG: String = s"M${start.x} ${start.y} A$rx $ry $xRot ${if (largeArc) 1 else 0} ${if (sweep) 1 else 0} ${end.x} ${end.y}"
}

/** A segment beginning at `a` and ending at `b`. */
case class Segment2(a: Vec2, b: Vec2) extends Shape2 {
  override def intersects(other: Shape2): Boolean = other match {
    case c: Circle2 =>
      (closestPointTo(c.c) - c.c).length <= c.r
    case s: Segment2 => intersection(s).isDefined
  }

  def intersection(other: Segment2): Option[Vec2] = {
    // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
    val dir = a -> b
    val otherDir = other.a -> other.b
    val dxo = dir cross otherDir
    if (dxo == 0) {
      if (((a -> other.a) cross dir) == 0) {
        // The two segments are collinear.
        val t0 = ((a -> other.a) dot dir) / (dir dot dir)
        val t1 = t0 + ((otherDir dot dir) / (dir dot dir))
        if ((t1 > t0 && t0 <= 1 && t1 >= 0) || (t1 <= t0 && t1 <= 1 && t0 >= 0))
          // collinear and intersecting, return average of all points so `a intersection b` == `b intersection a`
          Some((a + b + other.a + other.b) / 4)
        else
          // collinear but not intersecting
          None
      } else {
        // The two segments are parallel and non-intersecting.
        None
      }
    } else {
      val t = ((a -> other.a) cross otherDir) / dxo
      val u = ((a -> other.a) cross dir) / dxo
      if (0 <= t && t <= 1 && 0 <= u && u <= 1)
        Some(a + dir * t)
      else
        None
    }
  }

  /** The point on this segment closest to `p`. */
  def closestPointTo(p: Vec2): Vec2 = {
    val l2 = (a - b).lengthSquared
    if (l2 == 0) return a
    val t = Math.max(0, Math.min(1, ((p - a) dot (b - a)) / l2))
    a + (b - a) * t
  }

  def length: Double = (a -> b).length

  def toRectangle(width: Double): Polygon = {
    val sideways = (a -> b).perp.normed * (width / 2)
    Polygon(Seq(
      a + sideways,
      b + sideways,
      b - sideways,
      a - sideways
    ))
  }

  def sample(t: Double) = a.lerp(b, t)
  def toPoints(numPoints: Int): Seq[Vec2] = (0 until numPoints) map { i => sample(i/(numPoints - 1).toDouble) }

  def toSVG: String = {
    s"M${a.x},${a.y} L${b.x},${b.y}"
  }
}


/** Closed polygon. */
case class Polygon(points: Seq[Vec2]) extends Shape2 {
  /** Sequence of points representing the vertices of this polygon, with the final point equal to the first. */
  def toPolyLine: Seq[Vec2] = points :+ points.head

  /** Translate all the points in the polygon by `offset`. */
  def translate(offset: Vec2): Polygon = Polygon(points map (_ + offset))
  /** Rotate all the points in the polygon about Vec2(0, 0). */
  def rotateAroundOrigin(angle: Double): Polygon = Polygon(points map (_.rotate(angle)))
  /** Scale all the points in the polygon by `k`. */
  def scale(k: Double): Polygon = Polygon(points map (_ * k))

  def transform(mat: Mat33): Polygon = Polygon(points map (mat * _))

  // TODO: this might actually be isCW?
  def isCCW: Boolean = (points ++ points.takeRight(2)).sliding(3).forall {
    case Seq(a, b, c) =>
      ((a -> b) cross (b -> c)) <= 0
    case _ => true
  }

  def toCCWPolyLine = if (isCCW) toPolyLine else toPolyLine.reverse

  /** Area of the polygon, assuming its segments are non-intersecting. */
  def area: Double = (segments.foldLeft(0.0) { (a, s) => a + (s.a cross s.b) } / 2).abs

  /** A sequence of segments representing the edges of this polygon. */
  def segments = toPolyLine.sliding(2) map { case Seq(a, b) => Segment2(a, b) }

  /** The average of the vertices of this polygon. */
  def centroid: Vec2 = points.reduce(_ + _) / points.size

  override def intersects(other: Shape2): Boolean = other match {
    case seg: Segment2 =>
      segments.exists(_ intersects seg)
  }

  lazy val aabb = {
    var lowerX = points.head.x
    var lowerY = points.head.y
    var upperX = points.head.x
    var upperY = points.head.y
    for (p <- points.tail) {
      if (p.x < lowerX) lowerX = p.x
      if (p.x > upperX) upperX = p.x
      if (p.y < lowerY) lowerY = p.y
      if (p.y > upperY) upperY = p.y
    }
    AABB(Vec2(lowerX, lowerY), Vec2(upperX, upperY))
  }

  def toSVG: String = {
    val h = points.head
    val t = points.tail
    s"M${h.x},${h.y} ${t.map(p => s"L${p.x},${p.y}").mkString(" ")} Z"
  }
}

object Polygon {
  /** A square of side length `side` centered at the origin. */
  def square(side: Double): Polygon =
    rectangle(side, side)

  def rectangle(width: Double, height: Double): Polygon = {
    Polygon(Seq(
      Vec2(-width/2, -height/2),
      Vec2(width/2, -height/2),
      Vec2(width/2, height/2),
      Vec2(-width/2, height/2)
    ))
  }
}

object AABB {
  def apply(lowerX: Double, lowerY: Double, upperX: Double, upperY: Double): AABB =
    AABB(Vec2(lowerX, lowerY), Vec2(upperX, upperY))
}

/** Axis-aligned bounding box.
  *
  * `lower` must be <= `upper` in both dimensions.
  */
case class AABB(lower: Vec2, upper: Vec2) {
  require(lower.x <= upper.x && lower.y <= upper.y, s"Invalid AABB: $lower must be <= $upper")

  def width: Double = upper.x - lower.x
  def height: Double = upper.y - lower.y
  def maxDimension: Double = width max height
  def minDimension: Double = width min height
  def center: Vec2 = lower + (upper - lower) * 0.5

  def toPolygon: Polygon = Polygon(Seq(lower, lower.copy(x = upper.x), upper, lower.copy(y = upper.y)))

  // TODO: better names for the below
  private def ll = lower
  private def ul = Vec2(upper.x, lower.y)
  private def uu = upper
  private def lu = Vec2(lower.x, upper.y)

  private def topEdge = Segment2(ll, ul)
  private def rightEdge = Segment2(ul, uu)
  private def bottomEdge = Segment2(uu, lu)
  private def leftEdge = Segment2(lu, ll)

  /** The point at which `seg` intersects the edges of this AABB.
    * @todo not optimal.
    * @return `None` if `seg` is totally contained within or totally outside the box.
    *         <br>`Some(p)` where `p` is contained by the box otherwise.
    */
  def intersection(seg: Segment2): Option[Vec2] = {
    seg.intersection(leftEdge)
      .orElse(seg.intersection(topEdge))
      .orElse(seg.intersection(rightEdge))
      .orElse(seg.intersection(bottomEdge))
      .map(clip)
  }

  def intersects(other: AABB): Boolean = {
    lower.x <= other.upper.x && other.lower.x <= upper.x && lower.y <= other.upper.y && other.lower.y <= upper.y
  }

  def intersects(circle: Circle2): Boolean = {
    val Circle2(c, r) = circle
    c.x + r >= lower.x && c.x - r <= upper.x && c.y + r >= lower.y && c.y - r <= upper.y
  }

  /** True if `point` is contained within the AABB.
    *
    * Points exactly on the edge of the box are considered to be within the box.
    */
  def contains(point: Vec2): Boolean =
    point.x >= lower.x && point.x <= upper.x && point.y >= lower.y && point.y <= upper.y

  def contains(circle: Circle2): Boolean = {
    val Circle2(c, r) = circle
    c.x - r >= lower.x && c.x + r <= upper.x && c.y - r >= lower.y && c.y + r <= upper.y
  }

  def contains(polygon: Polygon): Boolean = polygon.points.forall(contains(_))

  /** Returns the largest subsegment that's completely contained within the AABB, if one exists. */
  def truncate(segment: Segment2): Option[Segment2] = {
    val aInside = contains(segment.a)
    val bInside = contains(segment.b)
    if (aInside && bInside)
      Some(segment)
    else if (aInside)
      Some(segment.copy(b = intersection(segment).getOrElse(segment.b)))
    else if (bInside)
      Some(segment.copy(a = intersection(segment).getOrElse(segment.a)))
    else
      None
  }

  def truncate(polygon: Polygon): Seq[Segment2] = polygon.segments.flatMap(truncate(_)).toSeq
  def truncate(circle: Circle2): Option[Either[Circle2, Arc2]] = {
    if (!intersects(circle)) None
    else if (contains(circle)) Some(Left(circle))
    else {
      ???
    }
  }

  /** Returns the closest point to `point` that's inside the AABB. */
  def clip(point: Vec2): Vec2 = Vec2(
    Math.max(lower.x, Math.min(upper.x, point.x)),
    Math.max(lower.y, Math.min(upper.y, point.y))
  )

  def expand(k: Double): AABB = AABB(lower - Vec2(k, k), upper + Vec2(k, k))
  def shrink(k: Double): AABB = AABB(lower + Vec2(k, k), upper - Vec2(k, k))
}
