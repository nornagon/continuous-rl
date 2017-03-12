package pcgtest

import kit.{Polygon, Vec2}
import kit.CanvasHelpers._
import org.scalajs.dom
import org.scalajs.dom.html

class SpacePacking {

  def spacePacking(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.translate(dom.window.innerWidth / 2, dom.window.innerHeight / 2)

    val rooms = Set(
      kit.pcg.SpacePacking.Room(
        Polygon.square(100),
        Seq(
          (Vec2(0, -50), -Math.PI/2),
          (Vec2(0, 50), Math.PI/2),
          (Vec2(-50, 0), Math.PI),
          (Vec2(50, 0), 0.0)
        )
      ),
      kit.pcg.SpacePacking.Room(
        Polygon.rectangle(40, 80),
        Seq(
          (Vec2(0, -40), -Math.PI/2),
          (Vec2(0, 40), Math.PI/2)
        )
      ),
      kit.pcg.SpacePacking.Room(
        Polygon.rectangle(200, 100),
        Seq(
          (Vec2(0, -50), -Math.PI/2),
          (Vec2(0, 50), Math.PI/2),
          (Vec2(-100, 0), Math.PI),
          (Vec2(100, 0), 0.0)
        )
      )
    )
    /*for (room <- rooms) {
      ctx.strokePath {
        ctx.polygon(room.outline)
        for (door <- room.doors) {
          ctx.moveTo(door._1)
          ctx.lineTo(door._1 + Vec2.forAngle(door._2) * 5)
        }
      }
    }*/
    val s = new kit.pcg.SpacePacking(rooms, (Vec2(0, 0), 0))
    ctx.fillPath { ctx.circle(Vec2(0, 0), 2) }
    for (i <- 1 to 20) {
      s.step()
    }
    for (room <- s.placedRooms) {
      ctx.strokePath("black") { ctx.polygon(room) }
    }
    for (d <- s.openDoors) {
      ctx.strokePath("orange") {
        ctx.moveTo(d._1)
        ctx.lineTo(d._1 + Vec2.forAngle(d._2) * 5)
      }
    }
  }
}
