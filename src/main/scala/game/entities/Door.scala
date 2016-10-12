package game.entities

import game.{DynamicEntity, World}
import kit.{Polygon, Shape2, Vec2, cp}


class Door(closedAngle: Double) extends DynamicEntity {
  override def mass: Double = 10

  override def castsShadow: Boolean = true

  override def update(world: World, dt: Double): Unit = {}

  override def shape: Seq[Shape2] = Seq(Polygon.rectangle(30, 4).translate(Vec2(15, 0)))

  override def didMount(world: World): Unit = {
    world.space.addConstraint(new cp.PivotJoint(body, world.space.staticBody, body.getPos()))
    world.space.addConstraint(new cp.RotaryLimitJoint(world.space.staticBody, body, closedAngle, closedAngle + Math.PI/2*1.3))
    body.setAngle(closedAngle)
    body.setAngVel(0)
    body.shapeList.foreach { s => s.group = 3 }
  }
}
