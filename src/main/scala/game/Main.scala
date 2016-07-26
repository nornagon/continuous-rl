package game

import kit._
import monix.execution.Ack
import monix.reactive.Observable
import monix.reactive.subjects.{BehaviorSubject, PublishSubject}
import org.scalajs.dom.html
import scala.concurrent.Future
import scala.scalajs.js.annotation.JSExport


trait Circuit {
  type State
  type Msg

  type View = Shaper[Msg] => Unit

  def initialState: State
  def update(state: State, msg: Msg): State
  def view(state: State): View

  val msgs = PublishSubject[Msg]()
  lazy val states: Observable[State] = initialState +: msgs.scan(initialState)(update)

  private val redraws = BehaviorSubject[Unit](())
  def redraw(): Future[Ack] = redraws onNext ()
  lazy val render: Observable[View] = (states combineLatestWith redraws) { (w, _) => w } map view
}

object Circuit {
  def run(circuit: Circuit, canvas: kit.Canvas): Unit = {
    import monix.execution.Scheduler.Implicits.global
    canvas.resizes.subscribe { _ => circuit.redraw() }
    circuit.render.subscribe { view =>
      canvas.ctx.clearRect(0, 0, canvas.width, canvas.height)
      view(new RenderShaper(canvas.ctx))
      Ack.Continue
    }
    Mouse.shapeHovers(canvas.ctx, circuit.render).subscribe { msg => circuit.msgs onNext msg }
    Mouse.shapeClicks(canvas.ctx, circuit.render).subscribe { msg => circuit.msgs onNext msg }
  }
}

@JSExport
object Main extends Circuit {
  case class State(
    playerLocation: (Double, Double),
    hovered: Option[String]
  )

  sealed trait Msg
  case class MovePlayer(dx: Double, dy: Double) extends Msg
  case class Hovered(id: Option[String]) extends Msg

  val initialState = State((0, 0), None)

  def update(world: State, msg: Msg): State = msg match {
    case MovePlayer(dx, dy) =>
      world.copy(playerLocation = (world.playerLocation._1 + dx, world.playerLocation._2 + dy))
    case Hovered(id) => world.copy(hovered = id)
  }

  def view(world: State): View = { (ctx: Shaper[Msg]) =>
    ctx.shape(
      ShapeProps(
        id = "player",
        fill = if (world.hovered.contains("player")) "red" else "blue",
        onMouseDown = { () => MovePlayer(0, 10) },
        onMouseOver = { () => Hovered(Some("player")) },
        onMouseOut = { () => Hovered(None) }
      )
    ) {
      _.rect(10 + world.playerLocation._1, 10 + world.playerLocation._2, 10, 10)
    }

    ctx.shape(
      ShapeProps(
        id = "house",
        fill = "green",
        onMouseDown = { () => MovePlayer(-world.playerLocation._1 + 10, -world.playerLocation._2 + 10) }
      )
    ) {
      _.rect(100, 100, 30, 30)
    }
  }

  @JSExport
  def main(root: html.Div): Unit = {
    import monix.execution.Scheduler.Implicits.global
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    val canvas = new kit.Canvas
    root.appendChild(canvas.element)
    Circuit.run(this, canvas)

    Keyboard.downs.subscribe { kc => msgs onNext MovePlayer(1, 0) }
  }
}
