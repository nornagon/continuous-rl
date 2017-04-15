package kit

import scala.collection.immutable.Seq
import scala.language.experimental.macros
import scala.meta._

class Reversible extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods def $name[..$tparams]($a, $b): $tpeopt = $expr" = defn
    val reversed = q"..$mods def $name[..$tparams]($b, $a): $tpeopt = $name(${Term.Name(a.name.value)}, ${Term.Name(b.name.value)})"
    Term.Block(Seq(defn, reversed))
  }
}

