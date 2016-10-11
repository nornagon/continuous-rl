package game

import kit._
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import kit.cp.Cp
import scala.scalajs.js
import cp.Implicits._
import scala.scalajs.js.JSConverters._

object Layer {
  val Background = -1
  val Ground = 0
  val Top = 1
}

trait Entity {
  def body: cp.Body
  def shape: Seq[Shape2]
  def castsShadow: Boolean = false
  def layers: Int = Cp.ALL_LAYERS
  final def pos: Vec2 = body.getPos()
  val layer = Layer.Ground
  val debugColor = "red"
  val debugLineWidth = 3
  def draw(ctx: CanvasRenderingContext2D) = {
    ctx.save {
      ctx.translate(pos.x, pos.y)
      ctx.rotate(body.a)
      shape foreach { s =>
        ctx.strokePath(debugColor, debugLineWidth) {
          s match {
            case c: Circle2 => ctx.circle(c.c, c.r)
            case p: Polygon => ctx.polygon(p)
          }
        }
      }
      if (shape.isEmpty) {
        ctx.strokePath(debugColor, debugLineWidth) {
          ctx.moveTo(-Vec2(3, 3))
          ctx.lineTo(Vec2(3, 3))
          ctx.moveTo(-Vec2(3, -3))
          ctx.lineTo(Vec2(3, -3))
        }
      }
    }
  }
  def didMount(world: World): Unit = {}
  def hit(world: World, other: Entity): Unit = {}
}

trait DynamicEntity extends Entity {
  def momentForShape(mass: Double, s: Shape2): Double = s match {
    case Circle2(c, r) => Cp.momentForCircle(mass, 0, r, c)
    case p: Polygon => Cp.momentForPoly(mass, p.toCCWPolyLine.flatMap(v => Seq(v.x, v.y)).toJSArray, Vec2(0, 0))
    case s: Segment2 => ???
  }
  // TODO: inertial moment is wrong for >1 shape
  val body = new cp.Body(mass, shape.map(momentForShape(mass, _)).reduceOption(_ + _).getOrElse(Double.PositiveInfinity))
  def mass: Double
  def update(world: World, dt: Double): Unit
}
trait StaticEntity extends Entity {
  val body = new cp.Body(Double.PositiveInfinity, Double.PositiveInfinity)
  body.asInstanceOf[js.Dynamic].nodeIdleTime = Double.PositiveInfinity
}

trait Incorporeal { self: Entity =>
  final def shape = Seq.empty
  final def mass = Double.PositiveInfinity
}
