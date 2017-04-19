package pcgtest

import kit._
import org.scalajs.dom
import org.scalajs.dom.{KeyboardEvent, html}
import snabbdom.{VNode, dsl => *}
import kit.RandomImplicits._

import scala.scalajs.js

class ThreeDee(page: AABB, seed: Int) {
  def three(root: html.Div): Unit = {
    val patch = snabbdom.snabbdom.init(js.Array(
      snabbdom.attributesModule,
      snabbdom.eventlistenersModule
    ))
    def render(a: Double): VNode = {
      val r = new scala.util.Random(seed)
      val proj = Mat44.translate(page.center.x, page.center.y, 0) * Mat44.scale(40) * Mat44.perspective(math.Pi/3, 1, 0.1, 100)
      val mv = Mat44.translate(0, 0, 4) * Mat44.rotate(a/3, a/2, 0, a) * Mat44.scale(2)
      val camera = proj * mv
      //val corners = for (x <- Seq(-1, 1); y <- Seq(-1, 1); z <- Seq(-1, 1)) yield Vec3(x, y, z)
      //val segs = for (a <- corners; b <- corners; if (a -> b).lengthSquared == 4) yield Segment3(a, b)
      val pointsOnSphere = (0 to 40) map { _ =>
        val u = r.nextDouble() * 2 - 1
        val t = r.angle
        val x = math.sqrt(1 - u*u) * math.cos(t)
        val y = math.sqrt(1 - u*u) * math.sin(t)
        val z = u
        Vec3(x, y, z)
      }
      /*
      val trisOnSphere = pointsOnSphere.flatMap { p =>
        val rng = r.between(0.6, 1) to r.between(1, 1.2) by 0.1
        val sz = r.between(0.05, 0.25)
        rng.map { h =>
          Circle3(p * h, sz * h * h, p).toPolygon(6)
        }
        //Seq(Circle3(p * r.between(0.9, 1), r.nextDouble() * 0.2 + 0.05, p).toPolygon(6))
      }
      val segs = trisOnSphere.flatMap(_.segments)
      */
      val segs: Seq[Segment3] = (0 to 500) map { _ =>
        val p = r.pick(pointsOnSphere)
        val o = r.nOf(15, pointsOnSphere).minBy(x => (x -> p).lengthSquared)
        Segment3(p, o)
      }
      /*
      val segs: Seq[Segment3] = (0 to 100) flatMap { _ =>
        val ps = r.nOf(7, pointsOnSphere)
        val circles = for (ai <- 0 until (ps.size-3); bi <- (ai+1) until ps.size; ci <- (bi+1) until ps.size) yield {
          val a = ps(ai)
          val b = ps(bi)
          val c = ps(ci)
          (a, b, c, Tri3(a, b, c).radius)
        }
        val (a, b, c, _) = circles.minBy(_._4)
        val center = (a + b + c) / 3
        val dn = center.normed * 0.3
        Seq(Segment3(a+dn, b+dn), Segment3(b+dn, c+dn), Segment3(c+dn, a+dn))
      }
      */
      val seg2s = segs map { s => Segment2((camera * s.a).toVec2, (camera * s.b).toVec2) }
      *.svg(
        *.xmlns := "http://www.w3.org/2000/svg",
        *.width := s"${page.width / 100.0}in",
        *.height := s"${page.height / 100.0}in",
        *.viewBox := s"0 0 ${page.width} ${page.height}",
        *.attr("x-seed") := seed.toString,
        *.g(
          seg2s.map(s => *.path(*.d := s.toSVG))
        )
      )
    }

    var x = 0.0d
    var prev: VNode = null
    val callback = (f: Double) => {
      if (prev != null)
        prev = patch(prev, render(x))
      else
        prev = patch(root, render(x))
      x += 0.003
    }
    var frameReq: Int = -1
    def paused = frameReq < 0
    def play(): Unit = {
      if (frameReq >= 0)
        dom.window.cancelAnimationFrame(frameReq)
      def frame(t: Double): Unit = {
        callback(t)
        frameReq = dom.window.requestAnimationFrame(frame _)
      }
      frameReq = dom.window.requestAnimationFrame(frame _)
    }
    def pause(): Unit = {
      dom.window.cancelAnimationFrame(frameReq)
      frameReq = -1
    }
    play()
    dom.window.onkeyup = (e: KeyboardEvent) => {
      if (e.keyCode == 0x20) {
        if (paused) play()
        else pause()
      }
    }
  }
}
