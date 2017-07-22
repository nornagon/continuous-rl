package snabbdom

import org.scalajs.dom.{Element, Text}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSImport.Namespace
import scala.scalajs.js.|

@JSImport("snabbdom", Namespace)
@js.native
object snabbdom extends js.Object {
  def init(modules: js.Array[js.Object]): js.Function2[VNode | Element, VNode, VNode] = js.native
}

@JSImport("snabbdom/h", Namespace)
@js.native
object h extends js.Function3[String, js.UndefOr[js.Any], js.UndefOr[js.Any], VNode] {
  def apply(selector: String, b: js.UndefOr[js.Any] = js.undefined, c: js.UndefOr[js.Any] = js.undefined): VNode = js.native
}

@JSImport("snabbdom/modules/class", Namespace)
@js.native
object classModule extends js.Object {}
@JSImport("snabbdom/modules/props", Namespace)
@js.native
object propsModule extends js.Object {}
@JSImport("snabbdom/modules/style", Namespace)
@js.native
object styleModule extends js.Object {}
@JSImport("snabbdom/modules/eventlisteners", Namespace)
@js.native
object eventlistenersModule extends js.Object {}
@JSImport("snabbdom/modules/attributes", Namespace)
@js.native
object attributesModule extends js.Object {}

@js.native
class VNode(
  val sel: js.UndefOr[String],
  val data: js.UndefOr[VNodeData],
  val children: js.UndefOr[js.Array[VNode | String]],
  val elm: js.UndefOr[Element | Text],
  val text: js.UndefOr[String],
  val key: js.Any
) extends js.Object

@js.native
class VNodeData private (
  attrs: js.UndefOr[js.Any],
  on: js.UndefOr[js.Any]
) extends js.Object

object VNodeData {
  def apply(
    attrs: js.UndefOr[js.Any],
    on: js.UndefOr[js.Any],
    hook: js.UndefOr[js.Any],
    ns: js.UndefOr[String] = js.undefined
  ): VNodeData = js.Dynamic.literal(
    attrs = attrs,
    on = on,
    hook = hook,
    ns = ns
  ).asInstanceOf[VNodeData]
}

object VNode {
  def vnode(
    sel: js.UndefOr[String] = js.undefined,
    data: js.UndefOr[VNodeData] = js.undefined,
    children: js.UndefOr[js.Array[VNode | String]] = js.undefined,
    //elm: js.UndefOr[Element | Text] = js.undefined,
    text: js.UndefOr[String] = js.undefined,
    key: js.Any = js.undefined
  ): VNode = js.Dynamic.literal(
    sel = sel,
    data = data,
    children = children,
    //elm = elm,
    text = text,
    key = key
  ).asInstanceOf[VNode]
}

