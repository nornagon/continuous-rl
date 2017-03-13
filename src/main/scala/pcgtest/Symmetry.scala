package pcgtest

import kit._
import kit.RandomImplicits._
import kit.{AABB, Segment2, Vec2}
import org.scalajs.dom.html

import scala.util.Random
import scalatags.JsDom.implicits._
import scalatags.JsDom.svgAttrs.{attr, d, fill, height, stroke, viewBox, width, xmlns}
import scalatags.JsDom.svgTags.{g, path, svg}

class Symmetry(page: AABB, _seed: Int) {
  private val margins = page.shrink(50)

  @Tweakable.Options
  case class Params(
    @Tweakable.Range(1, 10000)
    seed: Int = _seed,
    @Tweakable.Range(20, 1000)
    numSteps: Int = 100,
    @Tweakable.Range(1, 100)
    minWalk: Double = 5,
    @Tweakable.Range(1, 100)
    maxWalk: Double = 25,
    noBacksies: Boolean = true,
    @Tweakable.Range(0, 180)
    skewAngle: Double = 0,
    @Tweakable.Enum("rotational", "reflective", "rot-ref")
    symmetryType: String = "rotational",
    @Tweakable.Range(1, 8)
    symmetry: Int = 4
  )

  def tile(nX: Int, nY: Int)(f: (Int, Int) => Frag): Frag = {
    g((for (yi <- 0 until nY; xi <- 0 until nX) yield f(xi, yi)): _*)
  }

  def tileCentered(margins: AABB, nX: Int, nY: Int)(f: (Vec2) => Frag): Frag =
    tile(nX, nY) { (i, j) =>
      f(margins.lower + Vec2((margins.width / nX) * (i + 0.5), (margins.height / nY) * (j + 0.5)))
    }

  def rotationalSymmetry(n: Int)(ss: Seq[Segment2]): Seq[Seq[Segment2]] = {
    (0 until n) map { i =>
      val m = Mat33.rotate(i * Math.PI * 2 / n)
      ss map (seg => Segment2(m * seg.a, m * seg.b))
    }
  }

  def reflectiveSymmetry(axis: Double)(ss: Seq[Segment2]): Seq[Seq[Segment2]] = {
    val tx = Mat33.rotate(-axis) * Mat33.scale(1, -1) * Mat33.rotate(axis)
    Seq(
      ss,
      ss map { seg => Segment2(tx * seg.a, tx * seg.b) }
    )
  }

  def makeWalk(params: Params)(implicit r: Random): Seq[Segment2] = {
    import params._
    val ang = skewAngle / 360 * Math.PI * 2
    val gridTransform = Mat33.rotate(ang) * Mat33.scale(1, 0.5) * Mat33.rotate(-ang)
    val walk = (1 to numSteps).view
      .map(_ => gridTransform * r.oneOf(Vec2(-1, 0), Vec2(1, 0), Vec2(0, -1), Vec2(0, 1)))
        .sliding(2).filter { case Seq(a, b) => !noBacksies || a != -b }.map(_.head)
      .map(v => v * r.between(minWalk, maxWalk))
      .foldLeft(Seq.empty[Segment2]) {
        case (Seq(), p) => Seq(Segment2(Vec2(0, 0), p))
        case (h :: t, p) => Segment2(h.b, h.b + p) :: h :: t
      }

    walk
  }

  def symmetry(root: html.Div): Unit = {
    def render(params: Params): Unit = {
      implicit val r = new Random(params.seed)

      val art = tileCentered(margins, 3, 2) { center =>
        val walk = makeWalk(params)
        val symmetryFunction =
          if (params.symmetryType == "rotational")
            rotationalSymmetry(params.symmetry)(_)
          else if (params.symmetryType == "rot-ref")
            (segs: Seq[Segment2]) => reflectiveSymmetry(Math.PI/2)(segs).flatMap(pl => rotationalSymmetry(params.symmetry)(pl))
          else
            reflectiveSymmetry(Math.PI/2)(_)
        val walks = symmetryFunction(walk)
        val walksTranslated = walks.map(seg => seg.map(s => s.translate(center)).flatMap(margins.truncate(_)))
        val walksAsSVG = walksTranslated map { w =>
          s"M${w.head.a.x},${w.head.a.y} " + w.map { v => s"L${v.b.x},${v.b.y}"}.mkString(" ")
        }
        g(walksAsSVG.map(svg => path(d := svg, stroke := "black", fill := "transparent")): _*)
      }

      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        attr("x-seed") := s"${params.seed}",
        art
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }

    Params.tweakable()(render _)
  }
}
