package game

import org.scalajs.dom


//noinspection TypeAnnotation
object Assets {
  private def img(src: String) = {
    val i = dom.document.createElement("img").asInstanceOf[dom.raw.HTMLImageElement]
    i.src = s"/game/target/scala-2.12/classes/assets/$src"
    i
  }
  val tree = img("Tiles/tile_183.png")
  val blueMan = img("Man Blue/manBlue_stand.png")
  val blueManRightArm = img("man blue right arm.png")
  val gun = img("weapon_gun.png")
  val zombie1 = img("Zombie 1/zoimbie1_hold.png")
  val splatter = img("Tiles/tile_320.png")

  val square = img("squareWhite.png")
}

