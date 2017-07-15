package kit.pcg

import kit.Rand
import kit.Rand.Distribution
import scala.collection.mutable


/* possible extensions to WFC:
 * - infinite/lazy generation by just propagating out as far as it will go (or up to a limit)
 * - instead of simply eliminating some possibilities in a neighborhood, propagation could just make some things less likely
 * - modulate tile weights by a function (e.g. distance from center or Perlin noise)
 * - recursive backtracking to resolve contradictions
 */

class WaveFunctionCollapse {
  def entropy(probs: Iterable[Double]): Option[Double] = {
    // The probabilities in `probs` are not normalized (i.e. don't sum to 1), so we need to normalize first.
    // Derivation from Gibbs entropy for normalized p_i (S = -k_B \sum p_i \ln p_i):
    // S = -sum(normed_p_i ln normed_p_i)
    //   = -sum(p_i / norm * log(p_i / norm))
    //   = -sum(p_i * log(p_i / norm)) / norm
    //   = -sum(p_i * (log(p_i) - log(norm))) / norm
    //   = -sum(p_i * log(p_i) - p_i * log(norm)) / norm
    //   = (sum(p_i * log(norm)) - sum(p_i * log(p_i))) / norm
    //   = sum(p_i * log(norm)) / norm - sum(p_i * log(p_i)) / norm
    //   = sum(p_i / norm) * log(norm) - sum(p_i * log(p_i)) / norm
    //   = 1 * log(norm) - sum(p_i * log(p_i)) / norm
    //   = log(norm) - sum(p_i * log(p_i)) / norm
    val norm = probs.sum
    if (norm == 0)
      None
    else
      Some(Math.log(norm) - probs.foldLeft(0d)((sum, v) => sum + v * Math.log(v)) / norm)
  }

  def lowestEntropyAmong[Point, Tile](points: TraversableOnce[Point], f: Point => Distribution[Tile]): Point = {
    points.minBy(point => entropy(f(point).values))
  }

  def wfc(nx: Int = 20, ny: Int = 20): Map[(Int, Int), String] = {
    case class Tile(id: String, allowed: Map[(Int, Int), Set[String]])
    /*val tiles = Map(
      " " -> Tile(" ", Map((-1, 0) -> Set(" ", "|"), (1, 0) -> Set(" ", "|"), (0, -1) -> Set(" ", "-"), (0, 1) -> Set(" ", "-"))),
      "-" -> Tile("-", Map((-1, 0) -> Set("-", "+", " "), (1, 0) -> Set("-", "+", " "), (0, -1) -> Set(" ", "-"), (0, 1) -> Set(" ", "-"))),
      "|" -> Tile("|", Map((-1, 0) -> Set("|", " "), (1, 0) -> Set("|", " "), (0, -1) -> Set("|", "+", " "), (0, 1) -> Set("|", "+", " "))),
      "+" -> Tile("+", Map((-1, 0) -> Set("+", "-", " "), (1, 0) -> Set("+", "-", " "), (0, -1) -> Set("+", "|", " "), (0, 1) -> Set("+", "|", " ")))
    )*/
    implicit def stringToChars(s: String): Set[String] = s.split("").toSet
    val tiles = Map(
      "┐" -> Tile("┐", Map((-1, 0) -> "┌└", (1, 0) -> "┌└ ", (0, -1) -> "└┘ ", (0, 1) -> "└┘")),
      "┌" -> Tile("┌", Map((-1, 0) -> "┐┘ ", (1, 0) -> "┐┘", (0, -1) -> "└┘ ", (0, 1) -> "└┘")),
      "└" -> Tile("└", Map((-1, 0) -> "┐┘ ", (1, 0) -> "┐┘", (0, -1) -> "┌┐", (0, 1) -> "┌┐ ")),
      "┘" -> Tile("┘", Map((-1, 0) -> "┌└", (1, 0) -> "┌└ ", (0, -1) -> "┌┐", (0, 1) -> "┌┐ ")),
      " " -> Tile(" ", Map((-1, 0) -> "┐┘ ", (1, 0) -> "┌└ ", (0, -1) -> "└┘ ", (0, 1) -> "┌┐ "))
    )

    val grid = mutable.Map[(Int, Int), String]()
    val neighborhood = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
    def neighborPoints(point: (Int, Int)): Seq[(Int, Int)] =
      neighborhood map { case (x, y) => (point._1 + x, point._2 + y) }
    // Everything starts equally likely
    val undifferentiated = tiles.keySet.map(_ -> 1.0).toMap
    val wave = mutable.Map[(Int, Int), Distribution[String]]()
    for (x <- 0 until nx; y <- 0 until ny) wave((x, y)) = undifferentiated
    def observe(): Boolean = {
      if (wave.size == grid.size) return false
      val (point, possibilities) = (wave -- grid.keys).minBy { case (_, ps) => entropy(ps.values).get }
      grid(point) = Rand.chooseFrom(possibilities)
      propagate(point)
      true
    }

    def propagate(justChanged: (Int, Int)) = {
      val interesting = mutable.Queue[(Int, Int)]()
      interesting.enqueue(justChanged)
      while (interesting.nonEmpty) {
        val next = interesting.dequeue()
        val possibleTiles = if (grid.contains(next)) Set(tiles(grid(next))) else wave(next).keySet.map(tiles)
        val dirs = possibleTiles.flatMap(_.allowed.keySet)
        for (dir <- dirs; p = (next._1 + dir._1, next._2 + dir._2); if !grid.contains(p) && wave.contains(p)) {
          val allowed = possibleTiles.flatMap(_.allowed(dir))
          val disallowed = tiles.keySet -- allowed
          if (wave(p).keySet.exists(disallowed)) {
            wave(p) --= disallowed
            interesting.enqueue(p)
          }
        }
      }
    }
    while (observe()) ()
    for (y <- 0 until ny) {
      for (x <- 0 until nx) print(grid.getOrElse((x, y), "?"))
      println()
    }
    println()
    grid.toMap
  }
}
