package pcgtest

import kit.{Vec2, _}
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

@JSExport
object PCGTest extends JSApp {
  //val page = AABB(Vec2(0, 0), Vec2(1200, 900))
  val page = AABB(Vec2(0, 0), Vec2(1000, 700))
  //val page = AABB(Vec2(0, 0), Vec2(830, 580))

  val seed: Int = scala.util.Random.nextInt
  val r = new scala.util.Random(seed)

  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    //new Voronoi(page, seed).voronoiSVG(root)
    //new Symmetry(page, seed).symmetry(root)
    //new Horizon(page, seed).horizon(root)
    //new Substrate(page, seed).substrateSVG(root)
    new CircleThing(page, seed).circle(root)
  }

  def withCanvas(root: html.Div, f: html.Canvas => Unit): Unit = {
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth.toInt
    element.height = dom.window.innerHeight.toInt
    element.style.display = "block"
    root.appendChild(element)
    f(element)
  }

  override def main(): Unit = {
    main(dom.document.getElementById("appRoot").asInstanceOf[html.Div])
  }
}
