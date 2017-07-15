package kit


import kit.pcg.Noise
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}


class NoiseTest extends Properties("Noise") {
  implicit lazy val arbVec2: Arbitrary[Vec2] = Arbitrary {
    for {
      x <- arbitrary[Double]
      y <- arbitrary[Double]
    } yield Vec2(x, y)
  }

  property("within [-1, 1]") = forAll { (seed: Int) =>
    val noise = new Noise(seed)
    forAll { (v: Vec2) =>
      val n = noise.simplex2(v.x, v.y)
      -1 <= n && n <= 1
    }
  }
}
