package kit

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.raw.HTMLImageElement
import scala.scalajs.js


object CanvasHelpers {
  implicit class RichCanvasRenderingContext2D(ctx: CanvasRenderingContext2D) {
    def drawImage(img: HTMLImageElement, position: Vec2, scale: Double = 1.0): Unit = {
      ctx.drawImage(img, 0, 0, img.width, img.height, position.x, position.y, img.width * scale, img.height * scale)
    }

    def drawImageCentered(img: HTMLImageElement, position: Vec2, scale: Double = 1.0): Unit = {
      ctx.drawImage(
        img,
        0, 0,
        img.width, img.height,
        position.x - img.width * scale / 2, position.y - img.height * scale / 2,
        img.width * scale, img.height * scale
      )
    }

    def save[T](f: => T): T = {
      ctx.save()
      try {
        f
      } finally {
        ctx.restore()
      }
    }

    def transform(m: Mat33) = {
      ctx.transform(m.a, m.b, m.d, m.e, m.c, m.f)
    }

    def push(m: Mat33)(f: => Unit) = {
      save {
        transform(m)
        f
      }
    }

    def at(pos: Vec2, rotation: Double = 0.0)(f: => Unit) = {
      save {
        ctx.translate(pos.x, pos.y)
        ctx.rotate(rotation)
        f
      }
    }

    def moveTo(v: Vec2): Unit = {
      ctx.moveTo(v.x, v.y)
    }

    def lineTo(v: Vec2): Unit = {
      ctx.lineTo(v.x, v.y)
    }

    def polyLine(ps: Seq[Vec2]): Unit = {
      ctx.moveTo(ps.head)
      for (p <- ps.tail) ctx.lineTo(p)
    }

    def polygon(ps: Seq[Vec2]): Unit = {
      polyLine(ps)
      ctx.closePath()
    }

    def polygon(p: Polygon): Unit = polygon(p.points)

    def circle(c: Vec2, r: Double): Unit = {
      ctx.arc(c.x, c.y, r, 0, Math.PI * 2)
    }

    def fillPath(p: => Unit): Unit = {
      ctx.beginPath()
      p
      ctx.fill()
    }
    def fillPath(style: String)(p: => Unit): Unit = {
      ctx.fillStyle = style
      fillPath(p)
    }

    def fillPathEvenOdd(p: => Unit): Unit = {
      ctx.beginPath()
      p
      ctx.asInstanceOf[js.Dynamic].fill("evenodd")
    }

    def strokePath(p: => Unit): Unit = {
      ctx.beginPath()
      p
      ctx.stroke()
    }
    def strokePath(style: String, lineWidth: Double = 1)(p: => Unit): Unit = {
      ctx.strokeStyle = style
      ctx.lineWidth = lineWidth
      strokePath(p)
    }

    def clear() = ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
  }
}
