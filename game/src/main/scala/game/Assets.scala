package game

import org.scalajs.dom


object Assets {
  private def img(src: String) = {
    val i = dom.document.createElement("img").asInstanceOf[dom.raw.HTMLImageElement]
    i.src = src
    i
  }
  val tree = img("assets/Tiles/tile_183.png")
  val blueMan = img("assets/Man Blue/manBlue_stand.png")
  val blueManRightArm = img("assets/man blue right arm.png")
  val gun = img("assets/weapon_gun.png")
  val zombie1 = img("assets/Zombie 1/zoimbie1_hold.png")
  val splatter = img("assets/Tiles/tile_320.png")

  val square = img("assets/squareWhite.png")
}

