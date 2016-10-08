package game.actions

import game.{Assets, PlayerAction, World}


case class ReloadAction() extends PlayerAction {
  val duration = 1.2
  var t = 0.0
  override def update(world: World, dt: Double): Unit = {
    world.player.pose = Assets.blueManReload
    t += dt
    if (t >= duration) {
      world.player.ammo = 6
      world.player.pose = Assets.blueManGun
    }
  }

  override def isDone: Boolean = t >= duration
}

