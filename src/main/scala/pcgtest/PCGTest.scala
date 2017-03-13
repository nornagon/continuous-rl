package pcgtest

import kit.{Vec2, _}
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport

@JSExport
object PCGTest {
  val page = AABB(Vec2(0, 0), Vec2(1200, 900))
  //val page = AABB(Vec2(0, 0), Vec2(1000, 700))

  val seed: Int = scala.util.Random.nextInt
  val r = new scala.util.Random(seed)

  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    //new Voronoi(page, seed).voronoiSVG(root)
    new Symmetry(page, seed).symmetry(root)
  }

  def withCanvas(root: html.Div, f: html.Canvas => Unit): Unit = {
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth.toInt
    element.height = dom.window.innerHeight.toInt
    element.style.display = "block"
    root.appendChild(element)
    f(element)
  }
}
