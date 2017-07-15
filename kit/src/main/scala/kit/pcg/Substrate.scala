package kit.pcg

import kit.{Rand, Segment2, Vec2}
import scala.collection.mutable


case class SubstrateOptions(
  changeDirectionChance: Double = 0.01,
  changeDirectionAmount: Double = 0.1,
  changeDirectionMinSegLength: Double = 100,
  rightAngleChance: Double = 0.001,
  rightAngleNoise: Double = 0.1,
  rightAngleMinSegLength: Double = 20,
  maxRadius: Double = 200,
  maxSegmentLength: Double = Double.PositiveInfinity,
  chooseNewSegmentPosition: Segment2 => (Vec2, Double) = { parent =>
    val t = (Rand.between(0, parent.length) / 20).floor * 20
    val start = parent.a + (parent.a -> parent.b).normed * t
    val angle = (parent.a -> parent.b).toAngle + Rand.chooseFrom(-Math.PI/2 -> 1d, Math.PI/2 -> 1d, Rand.angle -> 0.02)
    (start, angle)
  }
)

class Substrate(
  sources: Set[(Vec2, Double)],
  val options: SubstrateOptions = SubstrateOptions()
) {
  val sourceSegments = sources.map { case (start, angle) => Segment2(start, start + Vec2.forAngle(angle)) }
  val deadSegments = mutable.Buffer.empty[Segment2]
  val liveSegments = mutable.Buffer[Segment2](sourceSegments.toSeq: _*)

  def allSegments = deadSegments ++ liveSegments

  def makeNewSegment(): Segment2 = {
    val parent = Rand.chooseFrom(allSegments.map(seg => seg -> seg.length).toMap)
    val (start, angle) = options.chooseNewSegmentPosition(parent)
    Segment2(start, start + Vec2.forAngle(angle))
  }

  def step(): Unit = {
    val died = mutable.Buffer.empty[Int]
    for (i <- liveSegments.indices) {
      val seg = liveSegments(i)
      val newSeg = seg.copy(b = seg.b + (seg.a -> seg.b).normed)
      liveSegments(i) = newSeg
      if (newSeg.b.length > options.maxRadius || newSeg.length > options.maxSegmentLength)
      //if (Math.random() > Math.pow(0.99998, liveSegments(i).length))
        died += i
      else if ((allSegments filter (newSeg ne _)).exists(seg.copy(a = seg.a + (seg.a -> seg.b).normed) intersects _)) {
        liveSegments(i) = seg
        died += i
      } else if (Math.random() < options.changeDirectionChance && seg.b.length > options.changeDirectionMinSegLength) {
        deadSegments.append(newSeg.copy(b = newSeg.b + (newSeg.b -> newSeg.a).normed * 0.1))
        liveSegments(i) = Segment2(
          a = newSeg.b,
          b = newSeg.b + Vec2.forAngle((newSeg.a -> newSeg.b).toAngle + Rand.between(-1.0, 1.0) * options.changeDirectionAmount)
        )
      } else if (Math.random() < options.rightAngleChance && newSeg.length > options.rightAngleMinSegLength) {
        deadSegments.append(newSeg.copy(b = newSeg.b + (newSeg.b -> newSeg.a).normed * 0.1))
        liveSegments(i) = Segment2(
          a = newSeg.b,
          b = newSeg.b + Vec2.forAngle((newSeg.a -> newSeg.b).toAngle + Rand.oneOf(-Math.PI/2, Math.PI/2) + Rand.between(-options.rightAngleNoise, options.rightAngleNoise))
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
