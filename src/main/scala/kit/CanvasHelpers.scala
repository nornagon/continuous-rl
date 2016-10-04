package kit

import org.scalajs.dom.CanvasRenderingContext2D
import scala.scalajs.js


object CanvasHelpers {
  implicit class RichCanvasRenderingContext2D(ctx: CanvasRenderingContext2D) {
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

    def circle(c: Vec2, r: Double): Unit = {
      ctx.arc(c.x, c.y, r, 0, Math.PI * 2)
    }

    def fillPath(p: => Unit) = {
      ctx.beginPath()
      p
      ctx.fill()
    }

    def fillPathEvenOdd(p: => Unit) = {
      ctx.beginPath()
      p
      ctx.asInstanceOf[js.Dynamic].fill("evenodd")
    }

    def strokePath(p: => Unit) = {
      ctx.beginPath()
      p
      ctx.stroke()
    }

    def clear() = ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
  }
}
