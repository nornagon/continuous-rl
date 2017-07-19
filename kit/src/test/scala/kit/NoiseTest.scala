package kit


import kit.pcg.Noise
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}


class NoiseTest extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  implicit lazy val arbVec2: Arbitrary[Vec2] = Arbitrary {
    for {
      x <- arbitrary[Double]
      y <- arbitrary[Double]
    } yield Vec2(x, y)
  }

  property("within [-1, 1]") {
    forAll { (seed: Int) =>
      val noise = new Noise(seed)
      forAll { (v: Vec2) =>
        val n = noise.simplex2(v.x, v.y)
        n should be (0d +- 1d)
      }
    }
  }
}
