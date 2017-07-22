package snabbdom

import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.|

object dsl {

  trait Modifier
  trait ChildModifier extends Modifier
  case class ChildVNodeModifier(child: VNode) extends ChildModifier
  case class ChildTextNodeModifier(child: String) extends ChildModifier
  case class ManyChildModifier(children: Iterable[ChildVNodeModifier]) extends ChildModifier
  implicit def vNodeToChildModifier(vnode: VNode): ChildVNodeModifier = ChildVNodeModifier(vnode)
  implicit def stringToChildModifier(str: String): ChildTextNodeModifier = ChildTextNodeModifier(str)
  implicit def iterableToChildrenModifier(children: Iterable[VNode]): ManyChildModifier = ManyChildModifier(children.map(ChildVNodeModifier))
  implicit def iterableToChildrenModifier(children: Option[VNode]): ManyChildModifier = ManyChildModifier(children.map(ChildVNodeModifier))

  case class AttributeModifier(attr: String, value: String) extends Modifier
  trait AttrPartial {
    def :=(n: String): AttributeModifier
  }
  trait RealAttrPartial extends AttrPartial {
    def :=(n: String): AttributeModifier
    def :=(n: Double): AttributeModifier = this.:=(n.toString)
  }
  def attr(k: String): AttrPartial = (n: String) => AttributeModifier(k, n)
  def realAttr(k: String): RealAttrPartial = (n: String) => AttributeModifier(k, n)

  val className = attr("class")
  val href = attr("href")
  val style = attr("style")
  val xmlns = attr("xmlns")
  val width = realAttr("width")
  val height = realAttr("height")
  val viewBox = attr("viewBox")
  val d = attr("d")
  val transform = attr("transform")
  val vectorEffect = attr("vector-effect")
  val cx = realAttr("cx")
  val cy = realAttr("cy")
  val r = realAttr("r")

  case class EventListenerModifier(event: String, listener: js.Function1[_ <: Event, _]) extends Modifier
  object onClick { def :=(f: js.Function1[MouseEvent, _]): EventListenerModifier = EventListenerModifier("click", f) }

  case class UpdateHookModifier(callback: js.Function2[VNode, VNode, _]) extends Modifier
  object hookUpdate { def :=(f: js.Function2[VNode, VNode, _]): UpdateHookModifier = UpdateHookModifier(f) }
  case class InsertHookModifier(callback: js.Function1[VNode, _]) extends Modifier
  object hookInsert { def :=(f: js.Function1[VNode, _]): InsertHookModifier = InsertHookModifier(f) }

  trait VNodeElement {
    def elementName: String
    def ns: js.UndefOr[String] = js.undefined
    def apply(mods: Modifier*): VNode = {
      val children = js.Array[VNode | String]()
      val attrs = js.Dictionary[String]()
      val listeners = js.Dictionary[js.Function1[_ <: Event, _]]()
      val hook = js.Dictionary[js.Any]()
      for (mod <- mods) {
        mod match {
          case ChildVNodeModifier(child) => children.push(child)
          case ChildTextNodeModifier(text) => children.push(VNode.vnode(text = text))
          case ManyChildModifier(cs) =>
            for (child <- cs) children.push(child.child)
          case AttributeModifier(attr, value) => attrs.update(attr, value)
          case EventListenerModifier(event, listener) => listeners.update(event, listener)
          case UpdateHookModifier(listener) => hook.update("update", listener)
          case InsertHookModifier(listener) => hook.update("insert", listener)
        }
      }
      VNode.vnode(
        elementName,
        data = VNodeData(attrs = attrs, on = listeners, ns = ns, hook = hook),
        children = children
      )
    }
  }
  trait SVGVNodeElement extends VNodeElement {
    final override val ns: js.UndefOr[String] = "http://www.w3.org/2000/svg"
  }

  object div extends VNodeElement { val elementName = "div" }
  object canvas extends VNodeElement { val elementName = "canvas" }
  object ul extends VNodeElement { val elementName = "ul" }
  object ol extends VNodeElement { val elementName = "ol" }
  object li extends VNodeElement { val elementName = "li" }
  object a extends VNodeElement { val elementName = "a" }
  object button extends VNodeElement { val elementName = "button" }
  object svg extends SVGVNodeElement { val elementName = "svg" }
  object g extends SVGVNodeElement { val elementName = "g" }
  object path extends SVGVNodeElement { val elementName = "path" }
  object circle extends SVGVNodeElement { val elementName = "circle" }
}
