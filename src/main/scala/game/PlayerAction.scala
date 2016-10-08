package game

trait PlayerAction {
  def update(world: World, dt: Double): Unit
  def isDone: Boolean
}

