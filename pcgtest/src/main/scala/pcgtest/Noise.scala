package pcgtest

import kit.AABB
import org.scalajs.dom
import org.scalajs.dom.html

class Noise(page: AABB, seed: Int) {
  def noise(canvas: html.Canvas): Unit = {
    val n = new kit.pcg.Noise(0)
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val image = ctx.createImageData(ctx.canvas.width, ctx.canvas.height)
    val data = image.data
    for (y <- 0 until ctx.canvas.height; x <- 0 until ctx.canvas.width) {
      val d = n.simplex2(x/50.0, y/50.0) // in [-1,1]
      val v: Int = ((d+1)/2 * 256).toInt
      val cell = (x + y * ctx.canvas.width) * 4
      data(cell) = v
      data(cell + 1) = v
      data(cell + 2) = v
      data(cell + 3) = 255
    }
    ctx.putImageData(image, 0, 0)
  }
}
