package pcgtest

import kit.{AABB, Vec2}
import org.scalajs.dom
import org.scalajs.dom.raw.MouseEvent
import org.scalajs.dom.{Element, html}
import snabbdom.{VNode, dsl => *}

import scala.scalajs.js

object PCGTest {
  //val page = AABB(Vec2(0, 0), Vec2(1200, 900))
  //val page = AABB(Vec2(0, 0), Vec2(1000, 700))
  val page = AABB(Vec2(0, 0), Vec2(830, 580))

  val seed: Int = scala.util.Random.nextInt
  val r = new scala.util.Random(seed)

  val experiments = Map(
    "voronoi" -> { (root: html.Div) => new Voronoi(page, seed).voronoiSVG(root) },
    "boxes" -> { (root: html.Div) => new Boxes(page, seed).boxes(root) },
    "threedee" -> { (root: html.Div) => new ThreeDee(page, seed).three(root) },
    "roomspring" -> { (root: html.Div) => new RoomSpring(page, seed).main(root) }
  )

  def main(root: html.Div): Unit = {
    val hash = dom.window.location.hash.substring(1)
    if (experiments contains hash) {
      experiments(hash)(root)
    } else {
      var newRoot: VNode = null
      val patch = snabbdom.snabbdom.init(js.Array(
        snabbdom.attributesModule,
        snabbdom.eventlistenersModule
      ))
      def render(): VNode = {
        *.div(
          "Choose:",
          *.ul(
            experiments.map {
              case (name, fn) =>
                *.li(*.a(name, *.onClick := { e: MouseEvent =>
                  newRoot.elm.asInstanceOf[Element].innerHTML = ""
                  dom.window.location.hash = name
                  fn(newRoot.elm.asInstanceOf[html.Div])
                }))
            }
          )
        )
      }
      val vd = patch(root, render())
      newRoot = vd
    }
    //new Voronoi(page, seed).voronoiSVG(root)
    //new Boxes(page, seed).boxes(root)
    //new Symmetry(page, seed).symmetry(root)
    //new Horizon(page, seed).horizon(root)
    //new Substrate(page, seed).substrateSVG(root)
    //new CircleThing(page, seed).circle(root)
    //new ThreeDee(page, seed).three(root)
    //new Balaban(page, seed).three(root)
    //new Writing(page).main(root)
    //new Particles(page, seed).particlesSVG(root)
    //new InterlockingGrids(page, seed).interlockingGrids(root)
    //new Cuts(page, seed).main(root)
    //new DeJong(page, seed).main(root)
    //new RoomSpring(page, seed).main(root)
  }

  def withCanvas(root: html.Div, f: html.Canvas => Unit): Unit = {
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth.toInt
    element.height = dom.window.innerHeight.toInt
    element.style.display = "block"
    root.appendChild(element)
    f(element)
  }

  def main(): Unit = {
    main(dom.document.getElementById("appRoot").asInstanceOf[html.Div])
  }
}
