package kit.cp

import kit.{AABB, Vec2}


object Implicits {
  implicit def Vec2toCpVect(v: Vec2): Vect = new Vect(v.x, v.y)
  implicit def cpVectToVec2(cpv: Vect): Vec2 = Vec2(cpv.x, cpv.y)

  implicit def cpBBToAABB(bb: BB): AABB = AABB(Vec2(bb.l, bb.b), Vec2(bb.r, bb.t))
  implicit def AABBTocpBB(bb: AABB): BB = new BB(bb.lower.x, bb.lower.y, bb.upper.x, bb.upper.y)
}
