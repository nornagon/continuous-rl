package kit.pcg

import kit.{Rand, Segment2, Vec2}
import scala.collection.mutable


class Substrate(sources: Set[(Vec2, Double)]) {
  val sourceSegments = sources.map { case (start, angle) => Segment2(start, start + Vec2.forAngle(angle)) }
  val deadSegments = mutable.Buffer.empty[Segment2]
  val liveSegments = mutable.Buffer[Segment2](sourceSegments.toSeq: _*)

  def allSegments = deadSegments ++ liveSegments

  def makeNewSegment(): Segment2 = {
    val parent = Rand.chooseFrom(allSegments.map(seg => seg -> seg.length).toMap)
    val t = (Rand.between(0, parent.length) / 20).floor * 20
    val start = parent.a + (parent.a -> parent.b).normed * t
    val angle = (parent.a -> parent.b).toAngle + Rand.chooseFrom(Map(-Math.PI/2 -> 1, Math.PI/2 -> 1, Rand.angle -> 0.02))
    Segment2(start + Vec2.forAngle(angle), start + Vec2.forAngle(angle) * 2)
  }

  def step(): Unit = {
    val died = mutable.Buffer.empty[Int]
    for (i <- liveSegments.indices) {
      val seg = liveSegments(i)
      val newSeg = seg.copy(b = seg.b + (seg.a -> seg.b).normed)
      liveSegments(i) = newSeg
      if (liveSegments(i).b.length > 200 + (deadSegments.size * 1.5))
      //if (liveSegments(i).length > 200)
      //if (Math.random() > Math.pow(0.99998, liveSegments(i).length))
        died += i
      else if ((allSegments filter (newSeg ne _)).exists(seg intersects _)) {
        liveSegments(i) = seg
        died += i
      } else if (Math.random() < 0.01 && seg.b.length > 100) {
        deadSegments.append(newSeg.copy(b = newSeg.b + (newSeg.b -> newSeg.a).normed * 0.1))
        liveSegments(i) = Segment2(
          a = newSeg.b,
          b = newSeg.b + Vec2.forAngle((newSeg.a -> newSeg.b).toAngle + Rand.between(-0.1, 0.1))
        )
      } else if (Math.random() < 0.001 && newSeg.length > 20) {
        deadSegments.append(newSeg.copy(b = newSeg.b + (newSeg.b -> newSeg.a).normed * 0.1))
        liveSegments(i) = Segment2(
          a = newSeg.b,
          b = newSeg.b + Vec2.forAngle((newSeg.a -> newSeg.b).toAngle + Rand.oneOf(-Math.PI/2, Math.PI/2) + Rand.between(-0.1, 0.1))
        )
      }
    }
    for (i <- died.reverse)
      deadSegments.append(liveSegments.remove(i))
    for (_ <- died.indices)
      for (_ <- 0 until Rand.oneOf(0, 1, 1, 1, 2, 2))
        liveSegments.append(makeNewSegment())
    liveSegments.remove(10, 10000)
  }
}
