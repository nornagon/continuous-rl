package kit

object Rand {
  def angle = Math.random() * 2 * Math.PI
  def between(a: Double, b: Double): Double = Math.random() * (b - a) + a
  def between(a: Int, b: Int): Int = (Math.random() * (b - a) + a).floor.toInt
  def oneIn(n: Int): Boolean = between(0, n) == 0

  /** A point uniformly sampled from the origin-centered circle with the given `radius`.
    * http://stackoverflow.com/questions/5837572/generate-a-random-point-within-a-circle-uniformly
    */
  def withinCircle(radius: Double = 1): Vec2 = {
    val u = Math.random() + Math.random() match { case x if x > 1 => 2 - x; case x => x }
    Vec2.forAngle(Rand.angle) * u * radius
  }
  def withinAABB(bounds: AABB): Vec2 =
    Vec2(
      Rand.between(bounds.lower.x, bounds.upper.x),
      Rand.between(bounds.lower.y, bounds.upper.y)
    )

  def oneOf[K](options: K*) = options(between(0, options.size).floor.toInt)
  def pick[K](options: Iterable[K]): K = {
    var chosen = options.head
    var i = 1
    for (o <- options.tail) {
      if (oneIn(i + 1))
        chosen = o
      i += 1
    }
    chosen
  }

  type Distribution[K] = Map[K, Double]

  def chooseFrom[K](distribution: Distribution[K]): K = {
    val norm = distribution.values.sum
    val r = Math.random() * norm
    var sum = 0d
    for ((t, p) <- distribution) {
      sum += p
      if (r < sum)
        return t
    }
    throw new RuntimeException(s"chooseFrom is wrong or $distribution is not a distribution")
  }
}
