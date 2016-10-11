package kit.cp

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import js.native


@JSName("cp.Vect")
@native
class Vect(var x: Double, var y: Double) extends js.Object

@JSName("cp.BB")
@native
class BB(var l: Double, var b: Double, var r: Double, var t: Double) extends js.Object

@JSName("cp.BBTree")
@native
class BBTree(staticIndex: js.Any) extends js.Object {
  def query(bb: BB, func: js.Function1[Shape, Unit]): Unit = native
}

@JSName("cp.Shape")
@native
class Shape(body: Body) extends js.Object {
  def setElasticity(e: Double): Unit = native
  def setFriction(u: Double): Unit = native
  def setLayers(layers: Int): Unit = native
  def setSensor(sensor: Boolean): Unit = native
  def setCollisionType(collisionType: Int): Unit = native
  def getBody(): Body = native
  def active(): Boolean = native
  def setBody(b: Body): Unit = native
  def cacheBB(): js.Any = native
  def getBB(): BB = native
  def update(pos: Vect, rot: Double): js.Any = native
  def pointQuery(p: Vect): js.UndefOr[NearestPointQueryInfo] = native
  var layers: Int = native
  var group: Int = native
  var bb_l: Double = native
  var bb_b: Double = native
  var bb_r: Double = native
  var bb_t: Double = native
}

@JSName("cp.PointQueryExtendedInfo")
@native
class PointQueryExtendedInfo(var shape: Shape) extends js.Object

@JSName("cp.NearestPointQueryInfo")
@native
class NearestPointQueryInfo(var shape: Shape, p: Vect, val d: Double) extends js.Object

@JSName("cp.SegmentQueryInfo")
@native
class SegmentQueryInfo(var shape: Shape, var t: Double, var n: Vect) extends js.Object

@JSName("cp.CircleShape")
@native
class CircleShape(body: Body, radius: Double, var offset: Vect) extends Shape(body) {
  var r: Double = native
}

@JSName("cp.SegmentShape")
@native
class SegmentShape(body: Body, var a: Vect, var b: Vect, var r: Double) extends Shape(body)

@JSName("cp.PolyShape")
@native
class PolyShape(body: Body, var verts: js.Array[Double], var offset: Vect) extends Shape(body)

@JSName("cp.BoxShape")
@native
object BoxShape extends js.Object{
  def apply(body: Body, width: Double, height: Double): PolyShape = native
}

@JSName("cp.Body")
@native
class Body(val m: Double, val i: Double) extends js.Object {
  var a: Double = native
  var f: Vect = native
  var t: Double = native
  var w: Double = native
  var shapeList: js.Array[Shape] = native

  def getPos(): Vect = native
  def getVel(): Vect = native
  def getAngVel(): Double = native
  def isSleeping(): Boolean = native
  def isStatic(): Boolean = native
  def isRogue(): Boolean = native
  def setMass(mass: Double): Unit = native
  def setMoment(moment: Double): Unit = native
  def addShape(shape: Shape): Shape = native
  def removeShape(shape: Shape): Unit = native
  def removeConstraint(constraint: js.Any): Unit = native
  def setPos(pos: Vect): Unit = native
  def setVel(velocity: Vect): Unit = native
  def setAngVel(w: Double): Unit = native
  def setAngle(angle: Double): Unit = native
  def applyForce(force: Vect, r: Vect): Unit = native
  def resetForces(): Unit = native
  def applyImpulse(j: Vect, r: Vect): Unit = native
  def getVelAtPoint(p: Vect): Vect = native
}

@JSName("cp.CollisionHandler")
@native
class CollisionHandler extends js.Object {
  var a: Shape = native
  var b: Shape = native
  var begin: (Arbiter, Space) => Boolean = native
  var preSolve: (Arbiter, Space) => Boolean = native
  var postSolve: (Arbiter, Space) => Unit = native
  var separate: (Arbiter, Space) => Boolean = native
}

@JSName("cp.Arbiter")
@native
class Arbiter extends js.Object{
  def getShapes(): Seq[Shape] = native
  def totalImpulse(): Vect = native
  def totalImpulseWithFriction(): Vect = native
  def totalKE(): Vect = native
  def ignore(): Unit = native
  def getA(): Shape = native
  def getB(): Shape = native
  def isFirstContact(): Boolean = native
}

@JSName("cp.Space")
@native
class Space() extends js.Object{
  type CollisionHandler = js.Function2[Arbiter, Space, Unit]
  def getCurrentTimeStep(): Double = native
  def setIterations(iter: Double): Unit = native
  def isLocked(): Boolean = native
  def addCollisionHandler(a: Int, b: Int, preSolve: CollisionHandler, postSolve: CollisionHandler, separate: CollisionHandler): Unit = native
  def removeCollisionHandler(a: Int, b: Int): Unit = native
  def setDefaultCollisionHandler(preSolve: CollisionHandler, postSolve: CollisionHandler, separate: CollisionHandler): Unit = native
  def lookupHandler(a: Int, b: Int): Double = native
  def addShape(shape: Shape): Shape = native
  def addStaticShape(shape: Shape): Shape = native
  def addBody(body: Body): Body = native
  def addConstraint(constraint: Constraint): Constraint = native
  def filterArbiters(body: Body, filters: js.Any): js.Any = native
  def removeShape(shape: Shape): Unit = native
  def removeStaticShape(shape: Shape): Unit = native
  def removeBody(body: Body): Unit = native
  def removeConstraint(constraint: js.Any): Unit = native
  def containsShape(shape: Shape): Boolean = native
  def containsBody(body: Body): Boolean = native
  def containsConstraint(constaint: js.Any): Boolean = native
  def step(dt: Double): Unit = native

  def pointQuery(point: Vect, layers: Int, group: Int, func: js.Function1[Shape, Unit]): Unit = native
  def pointQueryFirst(point: Vect, layers: Int, group: Int): Shape = native
  def segmentQuery(start: Vect, end: Vect, layers: Int, group: Int, func: js.Function3[Shape, Double, Vect, Unit]): Unit = native
  def segmentQueryFirst(start: Vect, end: Vect, layers: Int, group: Int): SegmentQueryInfo = native
  def bbQuery(bb: BB, layers: Int, group: Int, func: js.Function1[Shape, Unit]): Unit = native
  def shapeQuery(shape: Shape, func: js.Function2[Shape, js.Any, Unit]): Unit = native
  var damping: Double = native
  var gravity: Vect = native
  var staticBody: Body = native
  var bodies: js.Array[Body] = native
  var constraints: js.Array[Constraint] = native

  val activeShapes: BBTree = native
  val staticShapes: BBTree = native
}

@JSName("cp.Constraint")
@native
class Constraint(var a: Body, var b: Body) extends js.Object{
  def activateBodies(): js.Any = native
  var maxForce: Double = native
}

@JSName("cp.PinJoint")
@native
class PinJoint(a: Body, b: Body, anchr1: Vect, anchr2: Vect) extends Constraint(a, b)

@JSName("cp.SlideJoint")
@native
class SlideJoint(a: Body, b: Body, anchr1: Vect, anchr2: Vect, min: Double, max: Double) extends Constraint(a, b)

@JSName("cp.PivotJoint")
@native
class PivotJoint(a: Body, b: Body, var anchr1: Vect, var anchr2: Vect) extends Constraint(a, b)

@JSName("cp.GrooveJoint")
@native
class GrooveJoint(a: Body, b: Body, groove_a: Vect, groove_b: Vect, anchr2: Vect) extends Constraint(a, b)

@JSName("cp.DampedSpring")
@native
class DampedSpring(a: Body, b: Body, anchr1: Vect, anchr2: Vect, restLength: Double, stiffness: Double, damping: Double) extends Constraint(a, b)

@JSName("cp.DampedRotarySpring")
@native
class DampedRotarySpring(a: Body, b: Body, restAngle: Double, stiffness: Double, damping: Double) extends Constraint(a, b)

@JSName("cp.RotaryLimitJoint")
@native
class RotaryLimitJoint(a: Body, b: Body, min: Double, max: Double) extends Constraint(a, b)

@JSName("cp.RatchetJoint")
@native
class RatchetJoint(a: Body, b: Body, phase: js.Any, ratchet: js.Any) extends Constraint(a, b)

@JSName("cp.GearJoint")
@native
class GearJoint(a: Body, b: Body, phase: js.Any, ratio: Double) extends Constraint(a, b)

@JSName("cp.SimpleMotor")
@native
class SimpleMotor(a: Body, b: Body, rate: Double) extends Constraint(a, b)

