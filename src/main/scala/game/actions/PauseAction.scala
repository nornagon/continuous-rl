package game.actions

import game.{PlayerAction, World}


case class PauseAction(duration: Double) extends PlayerAction {
  var t = 0.0
  override def update(world: World, dt: Double): Unit = {
    t += dt
  }
  override def isDone: Boolean = t >= duration
}

