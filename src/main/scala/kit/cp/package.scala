package kit

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import js.native


package object cp {
  @JSName("cp")
  @native
  object Cp extends js.Object {
    def momentForBox(m: Double, width: Double, height: Double): Double = native
    def areaForPoly(verts: js.Array[Double]): Double = native
    def momentForPoly(m: Double, verts: js.Array[Double], offset: Vect): Double = native
    def momentForCircle(m: Double, r1: Double, r2: Double, offset: Vect): Double = native
    def centroidForPoly(verts: js.Array[Double]): Vect = native
    def recenterPoly(verts: js.Array[Double]): Unit = native

    def collideShapes(a: Shape, b: Shape): js.Array[Contact] = native

    val ALL_LAYERS: Int = native
  }

  def collideShapes(a: Shape, b: Shape): js.Array[Contact] = {
    if (a.collisionCode <= b.collisionCode)
      Cp.collideShapes(a, b)
    else {
      val cs = Cp.collideShapes(b, a)
      cs foreach { c => c.n.x = -c.n.x; c.n.y = -c.n.y }
      cs
    }
  }
}
