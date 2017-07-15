package pcgtest

import kit.{AABB, Vec2}
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html
import org.scalajs.dom.raw.SVGPathElement

import scala.scalajs.js

class Text(page: AABB, seed: Int) {
  def text(root: html.Div): Unit = {
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    case class Glyph(path: String, width: Double, height: Double)
    val url = "assets/isocpeur.svg"
    val glyphMapFuture = Ajax.get(url).map { xhr =>
      val glyphs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ:1234567890."
      val glyphSVGParent = dom.document.createElement("div")
      dom.document.body.appendChild(glyphSVGParent)
      glyphSVGParent.innerHTML = xhr.responseText
      val glyphSVG = glyphSVGParent.getElementsByTagName("svg")(0).asInstanceOf[dom.Element]
      val paths = glyphSVG.getElementsByTagName("path").asInstanceOf[js.Dynamic]
      val glyphMap: Map[Char, Glyph] = (for (g <- glyphs) yield {
        val codepoint = g.toInt
        val glyph = paths.selectDynamic(f"glyph-$codepoint%04X").asInstanceOf[SVGPathElement]
        val bbEl: dom.raw.SVGLocatable =
          if (glyph.hasAttribute("x-bb")) {
            glyphSVG.querySelector(glyph.getAttribute("x-bb")).asInstanceOf[dom.raw.SVGLocatable]
          } else
            glyph
        val bb = bbEl.getBBox()
        val seg = glyph.createSVGPathSegMovetoRel(-bb.x, -bb.y)
        glyph.pathSegList.insertItemBefore(seg, 0)
        g -> Glyph(path=glyph.getAttribute("d"), width=bb.width, height=bb.height)
      })(collection.breakOut)
      dom.document.body.removeChild(glyphSVGParent)
      glyphMap ++ Map(" " -> Glyph("", 20, 0))
    }
    val margins = page.shrink(100)
    for (glyphMap <- glyphMapFuture) {
      import scalatags.JsDom.implicits._
      import scalatags.JsDom.svgAttrs.{attr, d, fill, height, stroke, transform, viewBox, width, xmlns}
      import scalatags.JsDom.svgTags.{g, path, svg}
      def lineText(str: String, pos: Vec2, height: Double = 20.0) = {
        val xHeight = glyphMap('X').height
        val scale = height / xHeight
        var x = 0.0
        g(
          transform := s"translate(${pos.x},${pos.y}) scale($scale,$scale)",
          g(
            (for (char <- str) yield {
              val glyph = glyphMap.getOrElse(char.toUpper, Glyph("", 8, 0))
              val p = path(transform := s"translate($x, 0)", d := glyph.path, fill := "transparent", stroke := "black")
              x += glyph.width + 4
              p
            })(collection.breakOut): _*
          )
        )
      }
      val sizes = (1 to 10) map (_*2)
      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        attr("x-seed") := s"$seed",
        g(
          (for ((size, i) <- sizes.zipWithIndex) yield {
            lineText(s"size $size Sphinx of black quartz, judge my vow", margins.lower + Vec2(0, sizes.map(_+4).take(i).sum), size)
          })(collection.breakOut): _*
        )
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }
  }
}
