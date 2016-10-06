package kit.cp

import kit.Vec2


object Implicits {
  implicit def Vec2toCpVect(v: Vec2): Vect = new Vect(v.x, v.y)
  implicit def cpVectToVec2(cpv: Vect): Vec2 = Vec2(cpv.x, cpv.y)
}
