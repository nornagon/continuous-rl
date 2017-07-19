package kit


import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}


class BFSTest extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  property("finds when connected") {
    def links(n: Int) = Seq(n-1, n+1)
    BFS.find(0, links, (n: Int) => n >= 4) should contain (4)
  }

  property("doesn't find when not connected") {
    def links(n: Int) = if (math.abs(n) < 4) Seq(n-1, n+1) else Seq()
    BFS.find(0, links, (n: Int) => n == 9) shouldBe empty
  }

  property("path exists") {
    def links(n: Int) = Seq(n-1, n+1)
    BFS.path(0, links, (n: Int) => n >= 4) should contain (Seq(0, 1, 2, 3, 4))
  }

  val smallGridPoints = for (x <- Gen.choose(-10, 10); y <- Gen.choose(-10, 10)) yield (x, y)

  property("finds path in infinite grid") {
    def links(p: (Int, Int)) = p match { case (x, y) => Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)) }
    forAll(smallGridPoints) { (p: (Int, Int)) =>
      val path = BFS.path[(Int, Int)]((0, 0), links, _ == p)
      path should not be empty
      path.get should have size (math.abs(p._1) + math.abs(p._2) + 1)
      path.get.head shouldBe (0, 0)
      path.get.last shouldBe p
    }
  }
}
