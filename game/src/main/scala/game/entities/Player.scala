package game.entities

import game.actions.MoveAction
import game.items.Item
import game.{Assets, DynamicEntity, World}
import kit._
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import kit.cp.Implicits._


class Player extends DynamicEntity {
  override def mass = 10.0

  var held: Option[Item] = None

  def update(world: World, dt: Double): Unit = {
    world.currentAction match {
      case Some(action) =>
        if (!action.isInstanceOf[MoveAction])
          body.applyImpulse(-body.getVel() * 50 * dt, Vec2(0, 0))
      case _ =>
    }
  }

  var pointingAngle = 0.0

  def setTargetAngle(a: Double): Unit = {
    val delta = Angle.clipToPi(a - body.a)
    springBody.setAngle(body.a + delta)
  }
  private var springBody: cp.Body = _
  override def didMount(world: World): Unit = {
    springBody = new cp.Body(Double.PositiveInfinity, Double.PositiveInfinity)
    val rotation = new cp.DampedRotarySpring(springBody, body, 0, 6000, 2400)
    world.space.addConstraint(rotation)
  }

  override def shape: Seq[Shape2] = Seq(Circle2(Vec2(0,0), 10))

  def rightHandPos = pos + (rightArmAttachPoint.rotate(-body.a) + Vec2.forAngle(pointingAngle) * 6)

  val rightArmAttachPoint = Vec2(0, 7)
  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.at(pos, body.a) {
      val rightArmRotateCenter = Vec2(2, 4)
      for (item <- held) {
        ctx.at(rightArmAttachPoint, pointingAngle - body.a) {
          ctx.drawImage(
            Assets.blueManRightArm, 0, 0, Assets.blueManRightArm.width, Assets.blueManRightArm.height,
            -rightArmRotateCenter.x, -rightArmRotateCenter.y,
            Assets.blueManRightArm.width * 0.5, Assets.blueManRightArm.height * 0.5
          )
        }
        ctx.at(rightArmAttachPoint + Vec2.forAngle(pointingAngle - body.a) * 6, pointingAngle - body.a) {
          val gunAttachPoint = Vec2(2, 5)
          ctx.drawImage(
            Assets.gun, 0, 0, Assets.gun.width, Assets.gun.height,
            -gunAttachPoint.x, -gunAttachPoint.y,
            Assets.gun.width * 0.5, Assets.gun.height * 0.5
          )
        }
      }
      ctx.drawImageCentered(Assets.blueMan, Vec2(0, 0), 0.5)
    }
    if (grabbing != null) {
      ctx.strokePath("red", 2) {
        ctx.moveTo(grabbing.a.getPos() + grabbing.anchr1.rotate(-grabbing.a.a))
        ctx.lineTo(grabbing.b.getPos() + grabbing.anchr2.rotate(-grabbing.b.a))
      }
    }
  }

  var grabbing: cp.SlideJoint = _
  def grab(world: World, other: cp.Body, at: Vec2): Unit = {
    ungrab(world)
    grabbing = new cp.SlideJoint(
      body,
      other,
      rightArmAttachPoint,
      at,
      0,
      10
    )
    world.space.addConstraint(grabbing)
  }
  def ungrab(world: World): Unit = {
    if (grabbing != null)
      world.space.removeConstraint(grabbing)
    grabbing = null
  }
}

