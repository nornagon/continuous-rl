package game.actions

import game.items.Gun
import game.{PlayerAction, World}


case class ReloadAction(gun: Gun) extends PlayerAction {
  val duration = 8
  var t = 0.0
  override def update(world: World, dt: Double): Unit = {
    t += dt
    if (t >= duration) {
      gun.ammo = gun.props.maxAmmo
    }
  }

  override def isDone: Boolean = t >= duration
}

