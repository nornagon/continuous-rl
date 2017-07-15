import kit.{Segment2, Vec2}
import org.scalajs.dom

import scala.util.Random

package object pcgtest {
  def randomlyConnect(points: Seq[Vec2], factor: Double)(implicit r: Random): Seq[Segment2] =
    for (a <- points; b <- points; if a != b; if r.nextDouble() < factor) yield Segment2(a, b)

  def timed[T](label: String)(f: => T): T = {
    val start = dom.window.performance.now()
    val ret = f
    val end = dom.window.performance.now()
    val dur = end - start
    println(s"$label took $dur ms")
    ret
  }
}
