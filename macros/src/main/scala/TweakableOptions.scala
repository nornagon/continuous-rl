package kit

import scala.collection.immutable.Seq
import scala.meta._

package Tweakable {

  import scala.meta.Type.Name

  class Range(start: Double, end: Double) extends scala.annotation.StaticAnnotation {}
  class Enum(names: String*) extends scala.annotation.StaticAnnotation {}

  class Options extends scala.annotation.StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
      val q"case class $name(..$vs) extends $template" = defn

      val namedArgs = vs.map { v =>
        Term.Arg.Named(Term.Name(v.name.value), Term.Name(v.name.value))
      }
      val companion = q"""
        object ${Term.Name(name.value)} {
          def tweakable(..$vs)(trigger: $name => Unit): Unit = {
            var latest = ${Term.Name(name.value)}(..${vs.map(v => Term.Name(v.name.value))})
            val opts = _root_.scala.scalajs.js.Dynamic.literal(..$namedArgs)
            val gui = _root_.scala.scalajs.js.Dynamic.newInstance(_root_.scala.scalajs.js.Dynamic.global.dat.GUI)()
            ..${vs.map { v =>
              val extra = v.mods.collect {
                case mod"@Tweakable.Range($low, $high)" =>
                  Seq(low, high) ++ (
                    v.decltpe match {
                      case Some(t"Int") => Seq(Lit(1))
                      case _ => Seq()
                    }
                  )
                case mod"@Tweakable.Enum(..$opts)" =>
                  Seq(q"_root_.scala.scalajs.js.Array(..$opts)")
              }.flatten
              val argToUpdate = Term.Arg.Named(
                Term.Name(v.name.value),
                q"opts.selectDynamic(${Lit(v.name.value)}).asInstanceOf[${v.decltpe.get.asInstanceOf[Name]}]"
              )
              q"""
                gui.add(opts, ${Lit(v.name.value)}, ..$extra).onChange({ () =>
                  latest = latest.copy($argToUpdate)
                  trigger(latest)
                })
              """
            }}
            trigger(latest)
          }
        }
      """
      /*val template"{..$tstats} with ..$ctorcalls { ..$bstats }" = template
      val cls = q"""
        case class $name(..$vs) extends {..$tstats} with ..$ctorcalls {
          ..$bstats
        }
      """*/
      Term.Block(Seq(companion, defn))
    }
  }

}
