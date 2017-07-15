package pcgtest

import kit.{AABB, Circle2, Segment2, Vec2}
import kit.Tweakable
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.{FileReader, HTMLCanvasElement}

import scala.collection.mutable
import scalatags.JsDom.implicits._
import scalatags.JsDom.svgAttrs.{d, height, viewBox, width, xmlns}
import scalatags.JsDom.svgTags.{g, path, svg}

class Horizon(page: AABB, seed: Int) {
  implicit class RichImageData(imageData: ImageData) {
    def getChannel(i: Int, x: Int, y: Int): Int = {
      imageData.data((y * imageData.width + x) * 4 + i)
    }

    def getValue(x: Int, y: Int): Double = {
      val r = getChannel(0, x, y)
      val g = getChannel(1, x, y)
      val b = getChannel(2, x, y)
      (r + g + b) / 3.0 / 255
    }
  }

  def polylineToSVG(line: Seq[Segment2]): String = {
    val sb = new StringBuilder
    for (i <- line.indices) {
      val seg = line(i)
      val liftPen = i == 0 || (line(i-1).b -> seg.a).lengthSquared > 0.0001
      if (liftPen)
        sb.append(s"M${seg.a.x},${seg.a.y} ")
      sb.append(s"L${seg.b.x},${seg.b.y} ")
    }
    sb.mkString
  }

  @Tweakable.Options
  case class Params(
    @Tweakable.Range(0, 1)
    cutoff: Double = 0.5,
    invert: Boolean = false,
    lineSpacing: Int = 2,
    @Tweakable.Enum("even", "odd", "none")
    skipLines: String = "none"
  )

  def horizon(root: html.Div): Unit = {
    dom.document.ondragend = (e: DragEvent) => { e.preventDefault() }
    dom.document.ondragover = (e: DragEvent) => { e.preventDefault() }
    var currentImage: html.Image = null
    dom.document.ondrop = (e: DragEvent) => {
      e.preventDefault()
      val file = e.dataTransfer.files(0)
      val reader = new FileReader
      reader.onload = (e: UIEvent) => {
        val imgData = reader.result.asInstanceOf[String]
        val image = dom.document.createElement("img").asInstanceOf[dom.raw.HTMLImageElement]
        image.src = imgData
        currentImage = image
      }
      reader.readAsDataURL(file)
    }

    def resizeImageToHeight(image: html.Image, height: Int): ImageData = {
      val canvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
      val ratio = height / image.height.toDouble
      canvas.width = math.ceil(image.width * ratio).toInt
      canvas.height = math.ceil(image.height * ratio).toInt
      val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      ctx.drawImage(image, 0, 0, image.width, image.height, 0, 0, canvas.width, canvas.height)
      ctx.getImageData(0, 0, canvas.width, canvas.height)
    }

    def render(params: Params): Unit = {
      import params._
      if (currentImage == null) return
      val circ = Circle2(page.center, page.minDimension / 3)
      val numLines = math.floor(circ.r * 2 / lineSpacing).toInt
      val resizedImage = resizeImageToHeight(currentImage, numLines)
      val circleExtent = Segment2(circ.c + Vec2(0, -circ.r), circ.c + Vec2(0, circ.r))
      val lines = for (i <- 0 until numLines) yield {
        val d = circ.r * 2 * i / numLines.toDouble
        val p = circleExtent.a + Vec2(0, d)
        val xx = (circ.c -> p).lengthSquared
        val r = math.sqrt(1 - xx / (circ.r * circ.r)) * circ.r
        val line = Segment2(p - Vec2(r, 0), p + Vec2(r, 0))
        val segs = mutable.Buffer.empty[Segment2]
        for (x <- math.round(line.a.x).toInt to math.round(line.b.x).toInt by 1) {
          val normedX = math.round((x - circ.c.x + circ.r) / (circ.r * 2) * resizedImage.width.toDouble).toInt
          val v = resizedImage.getValue(normedX, i)
          if (if (invert) v > cutoff else v < cutoff) {
            if (segs.isEmpty || segs.last.b.x != x)
              segs.append(Segment2(Vec2(x, p.y), Vec2(x + 1, p.y)))
            else
              segs(segs.size - 1) = segs.last.copy(b = Vec2(x + 1, p.y))
          }
        }
        if (skipLines match { case "even" => i % 2 == 0; case "odd" => i % 2 == 1; case _ => false })
          Seq()
        else
          segs
      }

      val art = g(
        lines.map(seg => path(d := polylineToSVG(seg))): _*
      )

      val e = svg(
        xmlns := "http://www.w3.org/2000/svg",
        width := s"${page.width / 100.0}in",
        height := s"${page.height / 100.0}in",
        viewBox := s"0 0 ${page.width} ${page.height}",
        art
      ).render
      root.innerHTML = ""
      root.appendChild(e)
    }

    Params.tweakable()(render _)
  }
}
