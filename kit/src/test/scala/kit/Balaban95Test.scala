package kit

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class Balaban95Test extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  implicit lazy val arbVec2: Arbitrary[Vec2] = Arbitrary {
    for {
      x <- Gen.choose(-100, 100)
      y <- Gen.choose(-100, 100)
    } yield Vec2(x, y)
  }

  implicit lazy val arbSegment2: Arbitrary[Segment2] = Arbitrary {
    for {
      a <- Arbitrary.arbitrary[Vec2]
      b <- Arbitrary.arbitrary[Vec2]
    } yield Segment2(a, b)
  }

  implicit class FuzzyComparableVec2(v: Vec2) {
    def ~=(o: Vec2): Boolean = (v - o).lengthSquared <= 0.0000001
  }

  def ixsEqual(a: (Segment2, Segment2, Intersections.Intersection), b: (Segment2, Segment2, Intersections.Intersection)): Boolean = {
    (a._1 == b._1 && a._2 == b._2) || (a._1 == b._2 && a._2 == b._1) && (a._3 match {
      case Intersections.PointIntersection(p1) =>
        b._3 match {
          case Intersections.PointIntersection(p2) => p1 ~= p2
          case _ => false
        }
      case _ => false
    })
  }

  def ixsetsEqual(as: Seq[(Segment2, Segment2, Intersections.Intersection)], bs: Seq[(Segment2, Segment2, Intersections.Intersection)]): Boolean = {
    as.forall(ix => bs.exists(ixsEqual(_, ix))) && bs.forall(ix => as.exists(ixsEqual(_, ix)))
  }

  def ixMatch(right: Seq[(Segment2, Segment2, Intersections.Intersection)]) = new Matcher[Seq[(Segment2, Segment2, Intersections.Intersection)]] {
    override def apply(left: Seq[(Segment2, Segment2, Intersections.Intersection)]): MatchResult = {
      val result = MatchResult(
        ixsetsEqual(left, right),
        s"$left did not ixMatch $right",
        s"$left ixMatched $right"
      )
      result
    }
  }

  property("two segments") {
    forAll { (a: Segment2, b: Segment2) =>
      val balabanIxs = Balaban95.intersectingPairs(Seq(a, b))
      val naiveIxs = Balaban95.naiveIntersectingPairs(Seq(a, b))
      balabanIxs should ixMatch (naiveIxs)
    }
  }

  property("three segments") {
    forAll { (a: Segment2, b: Segment2, c: Segment2) =>
      val balabanIxs = Balaban95.intersectingPairs(Seq(a, b, c))
      val naiveIxs = Balaban95.naiveIntersectingPairs(Seq(a, b, c))
      balabanIxs should ixMatch (naiveIxs)
    }
  }

  property("N segments") {
    forAll { (segs: Seq[Segment2]) =>
      whenever(!segs.exists(s => s.a.x == s.b.x)) {
        val balabanIxs = Balaban95.intersectingPairs(segs)
        val naiveIxs = Balaban95.naiveIntersectingPairs(segs)
        balabanIxs should ixMatch (naiveIxs)
      }
    }
  }

  /*
  property("Vertical seg #1") = {
    val segs = Seq(
      Segment2(Vec2(32.00,-56.00),Vec2(91.00,44.00)),
      Segment2(Vec2(-1.00,40.00),Vec2(-1.00,53.00))
    )
    val balabanIxs = Balaban95.intersectingPairs(segs)
    val naiveIxs = Balaban95.naiveIntersectingPairs(segs)
    ixsetsEqual(balabanIxs, naiveIxs)
  }

  property("Vertical seg #2") = {
    val segs = Seq(
      Segment2(Vec2(-1.00,40.00),Vec2(-1.00,53.00))
    )
    val balabanIxs = Balaban95.intersectingPairs(segs)
    val naiveIxs = Balaban95.naiveIntersectingPairs(segs)
    ixsetsEqual(balabanIxs, naiveIxs)
  }
  */

  property("Proof that merge was broken") {
    val segs = Seq(
      Segment2(Vec2(-22.00, 61.00), Vec2(36.00, 12.00)),
      Segment2(Vec2(-7.00, -92.00), Vec2(32.00, -3.00)),
      Segment2(Vec2(-5.00, -14.00), Vec2(58.00, -52.00)),
      Segment2(Vec2(5.00, 95.00), Vec2(87.00, 9.00)),
      Segment2(Vec2(13.00, 46.00), Vec2(53.00, 30.00))
    )
    val balabanIxs = Balaban95.intersectingPairs(segs)
    val naiveIxs = Balaban95.naiveIntersectingPairs(segs)
    balabanIxs should ixMatch (naiveIxs)
  }

  property("Multiple segments appearing at the same x") {
    val segs = Seq(
      Segment2(Vec2(-28.00, -97.00), Vec2(52.00, 78.00)),
      Segment2(Vec2(-8.00, -36.00), Vec2(55.00, -42.00)),
      Segment2(Vec2(-8.00, 79.00), Vec2(93.00, 98.00))
    )
    val balabanIxs = Balaban95.intersectingPairs(segs)
    val naiveIxs = Balaban95.naiveIntersectingPairs(segs)
    balabanIxs should ixMatch (naiveIxs)
  }

  property("Start = finish") {
    val segs = Seq(
      Segment2(Vec2(36.00, 0.00), Vec2(64.00, -15.00)),
      Segment2(Vec2(64.00, -15.00), Vec2(83.00, -82.00))
    )
    val balabanIxs = Balaban95.intersectingPairs(segs)
    val naiveIxs = Balaban95.naiveIntersectingPairs(segs)
    balabanIxs should ixMatch (naiveIxs)
  }

  property("Bug with insertion") {
    val segs = Seq(
      Segment2(Vec2(-67.00, 98.00), Vec2(73.00, -60.00)),
      Segment2(Vec2(-67.00, 5.00), Vec2(20.00, -44.00))
    )
    val balabanIxs = Balaban95.intersectingPairs(segs)
    val naiveIxs = Balaban95.naiveIntersectingPairs(segs)
    balabanIxs should ixMatch (naiveIxs)
  }
}
