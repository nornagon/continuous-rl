package pcgtest

import kit.{AABB, Segment2, Vec2}
import org.scalajs.dom.html
import snabbdom.dsl.path
import snabbdom.{VNode, dsl => *}

import scala.scalajs.js

class Cuts(page: AABB, seed: Int) {
  private val margins = page.shrink(50)
  private val patch = snabbdom.snabbdom.init(js.Array(
    snabbdom.attributesModule,
    snabbdom.eventlistenersModule
  ))
  def render(): VNode = {
    val gridX = (page.width / 30).floor.toInt
    val gridY = (page.height / 30).floor.toInt
    val points = margins.subdivided(gridX, gridY)
    *.svg(
      *.xmlns := "http://www.w3.org/2000/svg",
      *.width := s"${page.width / 100.0}in",
      *.height := s"${page.height / 100.0}in",
      *.viewBox := s"0 0 ${page.width} ${page.height}",
      *.g(
        points.map(p =>
          path(
            *.d := Segment2(p - Vec2(10, 0), p + Vec2(10, 0)).toSVG
          )
        )
      )
    )
  }

  def main(root: html.Div): Unit = {
    patch(root, render())
  }
}
