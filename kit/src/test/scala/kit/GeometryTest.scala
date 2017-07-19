package kit

import kit.Intersections.{PointIntersection, SegmentIntersection}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}


class GeometryTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit lazy val arbVec2: Arbitrary[Vec2] = Arbitrary {
    for {
      x <- arbitrary[Double]
      y <- arbitrary[Double]
    } yield Vec2(x, y)
  }

  implicit lazy val arbVec3: Arbitrary[Vec3] = Arbitrary {
    for {
      x <- Gen.choose(-100, 100)
      y <- Gen.choose(-100, 100)
      z <- Gen.choose(-100, 100)
    } yield Vec3(x, y, z)
  }

  implicit lazy val arbVec4: Arbitrary[Vec4] = Arbitrary {
    for {
      x <- Gen.choose(-100, 100)
      y <- Gen.choose(-100, 100)
      z <- Gen.choose(-100, 100)
      w <- Gen.choose(-100, 100)
    } yield Vec4(x, y, z, w)
  }

  implicit lazy val arbMat33: Arbitrary[Mat33] = Arbitrary {
    for {
      a <- Gen.choose(-100, 100)
      b <- Gen.choose(-100, 100)
      c <- Gen.choose(-100, 100)
      d <- Gen.choose(-100, 100)
      e <- Gen.choose(-100, 100)
      f <- Gen.choose(-100, 100)
      g <- Gen.choose(-100, 100)
      h <- Gen.choose(-100, 100)
      i <- Gen.choose(-100, 100)
    } yield Mat33(a, b, c, d, e, f, g, h, i)
  }

  implicit lazy val arbMat44: Arbitrary[Mat44] = Arbitrary {
    for {
      a <- Gen.choose(-100, 100)
      b <- Gen.choose(-100, 100)
      c <- Gen.choose(-100, 100)
      d <- Gen.choose(-100, 100)
      e <- Gen.choose(-100, 100)
      f <- Gen.choose(-100, 100)
      g <- Gen.choose(-100, 100)
      h <- Gen.choose(-100, 100)
      i <- Gen.choose(-100, 100)
      j <- Gen.choose(-100, 100)
      k <- Gen.choose(-100, 100)
      l <- Gen.choose(-100, 100)
      m <- Gen.choose(-100, 100)
      n <- Gen.choose(-100, 100)
      o <- Gen.choose(-100, 100)
      p <- Gen.choose(-100, 100)
    } yield Mat44(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  }

  implicit class FuzzyComparableDouble(v: Double) {
    def ~=(o: Double): Boolean = (v - o).abs <= 0.0000001
  }
  implicit class FuzzyComparableVec2(v: Vec2) {
    def ~=(o: Vec2): Boolean = (v - o).lengthSquared <= 0.0000001
  }
  implicit class FuzzyComparableVec3(v: Vec3) {
    def ~=(o: Vec3): Boolean = (v - o).lengthSquared <= 0.0000001
  }
  implicit class FuzzyComparableVec4(v: Vec4) {
    def ~=(o: Vec4): Boolean = (v - o).lengthSquared <= 0.0000001
  }

  def approximatelyBe(right: Vec3): Matcher[Vec3] = (left: Vec3) => MatchResult(
    left ~= right,
    s"$left was not $right",
    s"$left was $right"
  )

  property("Vec22 scalar multiplication") {
    forAll { (a: Vec2, k: Double) =>
      val t = a * k
      t.x should equal (a.x * k)
      t.y should equal (a.y * k)
    }
  }

  property("Mat33 inverse") {
    forAll { (m: Mat33, v: Vec3) =>
      whenever (m.determinant != 0) {
        (m * m.inverse) * v should approximatelyBe (v)
        (m.inverse * m) * v should approximatelyBe (v)
      }
    }
  }

  property("Mat44 multiply") {
    forAll { (v: Vec4) => Mat44.identity * v shouldBe v }
  }

  private val smallDouble = Gen.choose(-100d, 100d)
  property("collinear segments on the X axis") {
    forAll(smallDouble, smallDouble, smallDouble, smallDouble) { (a: Double, b: Double, c: Double, d: Double) =>
      val seg0 = Segment2(Vec2(a, 0), Vec2(b, 0))
      val seg1 = Segment2(Vec2(c, 0), Vec2(d, 0))
      val intersections = Intersections.intersections(seg0, seg1)
      val points = Set(a, b, c, d)
      intersections match {
        case Seq() => true
        case Seq(PointIntersection(p)) => p.y shouldBe 0
        case Seq(SegmentIntersection(seg)) =>
          assert(points exists (_ ~= seg.a.x))
          assert(points exists (_ ~= seg.b.x))
      }
    }
  }
}
