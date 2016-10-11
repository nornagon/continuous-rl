package kit.pcg

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

  def wfc(n: Int = 20): Unit = {
    val grid = mutable.Map[(Int, Int), String]()
    val neighborhood = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
    def neighborPoints(point: (Int, Int)): Seq[(Int, Int)] =
      neighborhood map { case (x, y) => (point._1 + x, point._2 + y) }
    // Everything starts equally likely
    val undifferentiated = Set(" ", "+", "|", "-").map(_ -> 1.0).toMap
    val wave = mutable.Map[(Int, Int), Distribution[String]]().withDefaultValue(undifferentiated)
    val visited = mutable.Set[(Int, Int)]()
    val interesting = mutable.Set[(Int, Int)]((0, 0))
    def edge = interesting -- visited
  }
}
