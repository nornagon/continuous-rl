package kit.pcg

import kit.cp.Implicits._
import kit._
import scala.collection.mutable
import scala.scalajs.js.JSConverters._


object SpacePacking {
  case class Room(
    outline: Polygon,
    doors: Seq[(Vec2, Double)]
  )
}

class SpacePacking(rooms: Set[SpacePacking.Room], startingFrom: (Vec2, Double)) {

  val openDoors = mutable.Set[(Vec2, Double)](startingFrom)
  val placedRooms = mutable.Buffer.empty[Polygon]
  val cpShapes = mutable.Buffer.empty[cp.PolyShape]

  def step() = {
    val door = Rand.pick(openDoors)
    val room = Rand.pick(rooms)
    val roomDoor = Rand.pick(room.doors)

    val mat = Mat33.translate(door._1 + Vec2.forAngle(door._2)) * Mat33.rotate(-(door._2 + Math.PI - roomDoor._2)) * Mat33.translate(-roomDoor._1)
    val placed = room.outline.transform(mat)
    val cpShape = new cp.PolyShape(null, placed.toCCWPolyLine.flatMap(v => Seq(v.x, v.y)).toJSArray, Vec2(0, 0))
    cpShape.update(Vec2(0, 0), Vec2.forAngle(0))
    if (!cpShapes.exists(s => cp.collideShapes(s, cpShape).nonEmpty)) {
      openDoors.remove(door)
      placedRooms.append(placed)
      cpShapes.append(cpShape)
      for (d <- room.doors filter (_ != roomDoor)) {
        val od = (mat * d._1, d._2 + (door._2 + Math.PI - roomDoor._2))
        openDoors += od
      }
      (placed, true)
    } else (placed, false)
  }
}
