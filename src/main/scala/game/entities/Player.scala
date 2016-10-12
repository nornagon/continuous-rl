package game.entities

import game.actions.MoveAction
import game.{Assets, DynamicEntity, World}
import kit._
import org.scalajs.dom.CanvasRenderingContext2D
import kit.CanvasHelpers._
import kit.cp.Implicits._


class Player extends DynamicEntity {
  override def mass = 10.0
  var ammo = 6
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
      /*ctx.strokePath("red", lineWidth = 2) {
        ctx.moveTo(rightArmAttachPoint)
        ctx.lineTo(rightArmAttachPoint + Vec2.forAngle(pointingAngle - body.a) * 10)
      }*/
      ctx.drawImageCentered(Assets.blueMan, Vec2(0, 0), 0.5)
    }
    /*ctx.strokePath("yellow") {
      ctx.circle(rightHandPos, 2)
    }*/
  }
}

