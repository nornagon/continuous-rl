package kit

import monix.execution.Cancelable
import monix.reactive.{Observable, OverflowStrategy}
import org.scalajs.dom
import scala.scalajs.js


object Keyboard {
  type KeyCode = Int

  def downs: Observable[KeyCode] = DomWindow.listener("keydown") { (e: dom.KeyboardEvent) => e.keyCode }
  def ups: Observable[KeyCode] = DomWindow.listener("keyup") { (e: dom.KeyboardEvent) => e.keyCode }
  def presses: Observable[KeyCode] = DomWindow.listener("keypress") { (e: dom.KeyboardEvent) => e.keyCode }

}

object DomWindow {
  def listener[A, E <: dom.Event](event: String)(mapper: E => A): Observable[A] =
    Observable.create[A](OverflowStrategy.DropOld(2)) { downstream =>
      val listener: js.Function1[E, Unit] = { (e: E) =>
        downstream.onNext(mapper(e))
        ()
      }
      dom.window.addEventListener(event, listener)
      Cancelable { () => dom.window.removeEventListener(event, listener) }
    }
}
