package kit

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}


class FOVTest extends Properties("Geometry") {
  implicit lazy val arbVec2: Arbitrary[Vec2] = Arbitrary {
    for {
      x <- arbitrary[Double]
      y <- arbitrary[Double]
    } yield Vec2(x, y)
  }

  implicit lazy val arbSegment2: Arbitrary[Segment2] = Arbitrary {
    for {
      a <- arbitrary[Vec2]
      b <- arbitrary[Vec2]
    } yield Segment2(a, b)
  }

  property("self-comparison") = forAll { (a: Segment2, c: Vec2) =>
    !FOV.segmentInFrontOf(a, a, c)
  }

  property("antisymmetry") = forAll { (a: Segment2, b: Segment2, c: Vec2) =>
    FOV.segmentInFrontOf(a, b, c) == !FOV.segmentInFrontOf(b, a, c)
  }
}
