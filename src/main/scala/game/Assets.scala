package game

import org.scalajs.dom


object Assets {
  private def img(src: String) = {
    val i = dom.document.createElement("img").asInstanceOf[dom.raw.HTMLImageElement]
    i.src = src
    i
  }
  val tree = img("assets/Tiles/tile_183.png")
  val blueManGun = img("assets/Man Blue/manBlue_gun.png")
  val blueManReload = img("assets/Man Blue/manBlue_reload.png")
  val zombie1 = img("assets/Zombie 1/zoimbie1_hold.png")
  val splatter = img("assets/Tiles/tile_320.png")

  val square = img("assets/squareWhite.png")
}

