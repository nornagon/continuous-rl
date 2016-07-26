package kit

import monix.reactive.Observable
import org.scalajs.dom


object Mouse {
  def position: Observable[(Double, Double)] =
    DomWindow.listener("mousemove") { (e: dom.MouseEvent) => (e.clientX, e.clientY) }

  def clicks: Observable[(Double, Double)] =
    DomWindow.listener("click") { (e: dom.MouseEvent) => (e.clientX, e.clientY) }

  def hit[Msg](ctx: dom.CanvasRenderingContext2D, x: Double, y: Double)(view: Shaper[Msg] => Unit): Option[ShapeProps[Msg]] = {
    val hittester = new HitTestShaper[Msg](ctx, x, y)
    view(hittester)
    Option(hittester.lastHit)
  }

  def shapeHovers[Msg](ctx: dom.CanvasRenderingContext2D, render: Observable[Shaper[Msg] => Unit]): Observable[Msg] = {
    val hs =
      (position withLatestFrom render) { (pos: (Double, Double), view: Shaper[Msg] => Unit) =>
        val (x, y) = pos
        hit(ctx, x, y)(view)
      }.distinctUntilChangedByKey(_.map(_.id))
    hs.zip(hs.drop(1))
      .flatMap { h =>
        val (oldHover, newHover) = h
        Observable.fromIterable(Seq(
          oldHover flatMap { s => Option(s.onMouseOut) map { _() }},
          newHover flatMap { s => Option(s.onMouseOver) map { _() }}
        ).flatten)
      }
  }

  def shapeClicks[Msg](ctx: dom.CanvasRenderingContext2D, render: Observable[Shaper[Msg] => Unit]): Observable[Msg] = {
    (clicks withLatestFrom render) { (pos: (Double, Double), view: Shaper[Msg] => Unit) =>
      val (x, y) = pos
      hit(ctx, x, y)(view) flatMap (a => Option(a.onMouseDown) map (_()))
    }.flatMap(Observable.fromIterable(_))
  }
}
