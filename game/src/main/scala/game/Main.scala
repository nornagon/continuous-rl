package game

import game.entities.{Building, Road, ZombieSpawner}
import game.items._
import kit._
import kit.pcg.PoissonDisk
import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js


object Main {
  def makeTestWorld(): World = {
    val world = new World
    world.screenSize = Vec2(ctx.canvas.width, ctx.canvas.height)
    world.addEntity(new entities.Building(Circle2(Vec2(0, 0), 40).toPolygon(4)), Vec2(100, 0))
    world
  }
  val gunDef = ItemDef(
    id = "gun",
    name = "A pistol",
    componentProps = Seq(
      GunProperties(6)
    )
  )
  val ammoDef = ItemDef(
    id = "ammo",
    name = "A bullet",
    componentProps = Seq(
      AmmoProperties("9mm")
    )
  )
  val magazineDef = ItemDef(
    id = "magazine",
    name = "A magazine which holds bullets",
    componentProps = Seq(
      ContainerProperties(maxVolume = "200:milliliter")
    )
  )
  def makeMagazine(ammoDef: ItemDef, count: Int): Item = {
    val mag = Item.fromDefinition(magazineDef)
    for (_ <- 1 to count)
      mag[Container].containedItems.append(Item.fromDefinition(ammoDef))
    mag
  }
  def makeWorld(): World = {
    val world = new World
    world.player.held = Some(Item.fromDefinition(gunDef))
    for (i <- 1 to 4) {
      world.addEntity(new entities.Zombie, Vec2(Math.random() * 1000 - 500, Math.random() * 1000 - 500))
    }
    world.screenSize = Vec2(ctx.canvas.width, ctx.canvas.height)
    world.addEntity(new ZombieSpawner, Vec2(0, 0))

    // Generate roads
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

    // Generate houses
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
          val wallWidth = 8
          if (i == 3) {
            val t = Rand.between(0.2, 0.8) * seg.length
            val dir = (seg.a -> seg.b).normed
            val wall1 = Segment2(seg.a, seg.a + dir * (t - 15))
            val wall2 = Segment2(seg.b, seg.a + dir * (t + 15))
            world.addEntity(new Building(wall1.toRectangle(wallWidth)), Vec2(0, 0))
            world.addEntity(new Building(wall2.toRectangle(wallWidth)), Vec2(0, 0))
            world.addEntity(new entities.Door(dir.toAngle), seg.a + dir * (t - 15))
          } else {
            world.addEntity(new Building(seg.toRectangle(wallWidth)), Vec2(0, 0))
          }
        }
        for (i <- 0 until Rand.between(0, 4)) {
          world.addEntity(new entities.ItemEntity(null), pointNearRoad + Rand.withinCircle(300))
        }
      }
    }

    // Generate trees
    PoissonDisk.generate(AABB(-10000, -10000, 10000, 10000), 400)(new scala.util.Random()) foreach { point =>
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

  def main(root: html.Div): Unit = {

    val itemDef = ItemDef(
      componentProps = Seq(
        WearableProperties("torso", 1, 1)
      ),
      name = "T-shirt",
      id = "tshirt"
    )
    val item = Item.fromDefinition(itemDef)
    println(item[Wearable].props.location)


    root.innerHTML = ""  // Otherwise workbench update doesn't work properly
    val element = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = dom.window.innerWidth.toInt
    element.height = dom.window.innerHeight.toInt
    element.style.display = "block"
    root.appendChild(element)
    ctx = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    world = makeWorld()
    //world = makeTestWorld()
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

  def main(): Unit = {
    main(dom.document.getElementById("appRoot").asInstanceOf[html.Div])
  }
}
