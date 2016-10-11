package kit

object Rand {
  def angle = Math.random() * 2 * Math.PI
  def between(a: Double, b: Double) = Math.random() * (b - a) + a

  def oneOf[K](options: K*) = options(between(0, options.size).floor.toInt)

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
