package game

import game.entities.{Building, Road, ZombieSpawner}
import kit._
import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport


@JSExport
object Main {
  def makeWorld(): World = {
    val world = new World
    for (i <- 1 to 4) {
      world.addEntity(new entities.Zombie, Vec2(Math.random() * 1000 - 500, Math.random() * 1000 - 500))
    }
    world.screenSize = Vec2(ctx.canvas.width, ctx.canvas.height)
    world.addEntity(new ZombieSpawner, Vec2(0, 0))
    val s = new kit.pcg.Substrate(Set((Vec2(0, 0), 0.0)))
    while (s.deadSegments.size < 200) {
      s.step()
      if (s.liveSegments.isEmpty)
        s.liveSegments.append(s.makeNewSegment())
    }
    val roads = s.deadSegments.map { s => Segment2(s.a * 100, s.b * 100) }
    for (seg <- roads) {
      world.addEntity(new Road(seg), Vec2(0, 0))
    }
    for (_ <- 1 to roads.size * 4) {
      val seg = Rand.oneOf(roads: _*)
      val t = Rand.between(0, seg.length / 500).floor * 500
      val pointOnRoad = seg.a + (seg.a -> seg.b).normed * t
      val angle = (seg.a -> seg.b).toAngle + Math.PI / 2 * Rand.oneOf(-1, 1)
      val pointNearRoad = pointOnRoad + Vec2.forAngle(angle) * 500
      val poly = Polygon.square(800).rotateAroundOrigin(-angle).translate(pointNearRoad)
      if (!roads.exists(poly intersects _)) {
        val smallerPoly = Polygon.square(600).rotateAroundOrigin(-angle).translate(pointNearRoad)
        for ((seg, i) <- smallerPoly.segments.zipWithIndex) {
          if (i == 3) {
            val t = Rand.between(0.2, 0.8) * seg.length
            val dir = (seg.a -> seg.b).normed
            val wall1 = Segment2(seg.a, seg.a + dir * (t - 15))
            val wall2 = Segment2(seg.b, seg.a + dir * (t + 15))
            world.addEntity(new Building(wall1.toRectangle(8)), Vec2(0, 0))
            world.addEntity(new Building(wall2.toRectangle(8)), Vec2(0, 0))
            world.addEntity(new entities.Door(dir.toAngle), seg.a + dir * (t - 15))
          } else {
            world.addEntity(new Building(seg.toRectangle(8)), Vec2(0, 0))
          }
        }
      }
    }
    for (_ <- 1 to Rand.between(roads.size * 4, roads.size * 8)) {
      val point = Rand.withinCircle(radius = 10000)
      val touching = roads.exists(seg => (seg.closestPointTo(point) -> point).length < 100)
      if (!touching) {
        val tree = world.addEntity(new entities.Tree, point)
        tree.body.setAngle(Math.random() * Math.PI / 2)
      }
    }
    world
  }
  var world: World = _
  var ctx: dom.CanvasRenderingContext2D = _

  def frame(t: Double): Unit = {
    world.processInput()
    world.update()
    world.draw(ctx)
    if (world.gameOver) {
      dom.window.alert("The zombie's grip is tight as death. Its teeth bite down on your flesh and you realise it's the end...")
      world = makeWorld()
    }
  }

  def run(): Unit = {
    def loop(t: Double): Unit = {
      frame(t)
      dom.window.requestAnimationFrame(loop _)
    }
    dom.window.requestAnimationFrame(loop _)
  }

  @JSExport
  def main(root: html.Div): Unit = {
    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth
    element.height = dom.window.innerHeight
    element.style.display = "block"
    root.appendChild(element)
    ctx = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    world = makeWorld()
    dom.window.addEventListener("keydown", (e: KeyboardEvent) => {
      world.codesDown += e.asInstanceOf[js.Dynamic].code.asInstanceOf[String]
    })
    dom.window.addEventListener("keyup", (e: KeyboardEvent) => {
      world.codesDown -= e.asInstanceOf[js.Dynamic].code.asInstanceOf[String]
    })
    dom.window.addEventListener("blur", (e: FocusEvent) => {
      world.codesDown = Set.empty
    })
    dom.window.addEventListener("mousemove", (e: MouseEvent) => {
      world.mouseScreenPos = Vec2(e.clientX, e.clientY)
    })
    dom.window.addEventListener("mousedown", (e: MouseEvent) => {
      world.mouseScreenPos = Vec2(e.clientX, e.clientY)
    })
    dom.window.addEventListener("mouseup", (e: MouseEvent) => {
      world.mouseScreenPos = Vec2(e.clientX, e.clientY)
    })
    run()
  }
}
