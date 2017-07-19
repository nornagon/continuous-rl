package kit


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
}
