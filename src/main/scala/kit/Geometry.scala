package kit

case class Vec2(x: Double, y: Double) {
  def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
  def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
  def *(k: Double): Vec2 = Vec2(x * k, y * k)
  def /(k: Double): Vec2 = Vec2(x / k, y / k)

  def dot(other: Vec2): Double = x * other.x + y * other.y
  def cross(other: Vec2): Double = x * other.y - other.x * y

  def ->(other: Vec2): Vec2 = other - this

  def lengthSquared = x * x + y * y
  def length = Math.sqrt(lengthSquared)
  def normed = if (length == 0) Vec2(0, 0) else this / length

  def toAngle = Math.atan2(y, x)

  def perp = Vec2(-y, x)
}

object Vec2 {
  def forAngle(t: Double) = Vec2(Math.cos(t), Math.sin(t))
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
  def rotate(theta: Double): Mat33 = {
    val c = Math.cos(theta)
    val s = Math.sin(theta)
    Mat33(
      c, s, 0,
      -s, c, 0,
      0, 0, 1
    )
  }
}

trait Shape2 {
  def intersects(other: Shape2): Boolean
}

case class Circle2(c: Vec2, r: Double) extends Shape2 {
  override def intersects(other: Shape2): Boolean = other match {
    case other: Circle2 => (c - other.c).length <= r + other.r
    case o => o intersects this
  }
}

case class Segment2(a: Vec2, b: Vec2) extends Shape2 {
  override def intersects(other: Shape2): Boolean = other match {
    case c: Circle2 =>
      (closestPointTo(c.c) - c.c).length <= c.r
    case s: Segment2 =>
      // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
      val dir = a -> b
      val otherDir = s.a -> s.b
      val dxo = dir cross otherDir
      if (dxo == 0) {
        if (((a -> s.a) cross dir) == 0) {
          // The two segments are collinear.
          val t0 = ((a -> s.a) dot dir) / (dir dot dir)
          val t1 = t0 + ((otherDir dot dir) / (dir dot dir))
          if (t1 > t0)
            t0 <= 1 && t1 >= 0
          else
            t1 <= 1 && t0 >= 0
        } else {
          // The two segments are parallel and non-intersecting.
          false
        }
      } else {
        val t = ((a -> s.a) cross otherDir) / dxo
        val u = ((a -> s.a) cross dir) / dxo
        0 <= t && t <= 1 && 0 <= u && u <= 1
      }
  }

  def closestPointTo(p: Vec2): Vec2 = {
    val l2 = (a - b).lengthSquared
    if (l2 == 0) return a
    val t = Math.max(0, Math.min(1, ((p - a) dot (b - a)) / l2))
    a + (b - a) * t
  }
}
