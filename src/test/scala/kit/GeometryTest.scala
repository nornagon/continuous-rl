package kit

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}


class GeometryTest extends Properties("Geometry") {
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

  implicit class FuzzyComparableVec3(v: Vec3) {
    def ~=(o: Vec3): Boolean = (v - o).lengthSquared <= 0.0000001
  }

  property("Vec22 scalar multiplication") = forAll { (a: Vec2, k: Double) =>
    val t = a * k
    t.x == a.x * k && t.y == a.y * k
  }

  property("Mat33 inverse") = forAll { (m: Mat33, v: Vec3) =>
    (m.determinant != 0) ==> {
      ((m * m.inverse) * v ~= v) &&
        ((m.inverse * m) * v ~= v)
    }
  }
}
