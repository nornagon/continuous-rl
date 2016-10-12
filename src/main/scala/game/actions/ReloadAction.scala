package game.actions

import game.{PlayerAction, World}


case class ReloadAction() extends PlayerAction {
  val duration = 8
  var t = 0.0
  override def update(world: World, dt: Double): Unit = {
    t += dt
    if (t >= duration) {
      world.player.ammo = 6
    }
  }

  override def isDone: Boolean = t >= duration
}

