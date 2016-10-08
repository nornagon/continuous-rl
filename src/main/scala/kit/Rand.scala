package kit

object Rand {
  def angle = Math.random() * 2 * Math.PI
  def between(a: Double, b: Double) = Math.random() * (b - a) + a
}
