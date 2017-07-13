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

  case class AttributeModifier(attr: String, value: String) extends Modifier
  trait AttrPartial {
    def :=(n: String): AttributeModifier
  }
  trait RealAttrPartial extends AttrPartial {
    def :=(n: String): AttributeModifier
    def :=(n: Double): AttributeModifier = this.:=(n.toString)
  }
  def attr(k: String): AttrPartial = new AttrPartial {
    override def :=(n: String) = AttributeModifier(k, n)
  }
  def realAttr(k: String): RealAttrPartial = new RealAttrPartial {
    override def :=(n: String) = AttributeModifier(k, n)
  }
  val className = attr("class")
  val href = attr("href")
  val xmlns = attr("xmlns")
  val width = attr("width")
  val height = attr("height")
  val viewBox = attr("viewBox")
  val d = attr("d")
  val transform = attr("transform")
  val vectorEffect = attr("vector-effect")
  val cx = realAttr("cx")
  val cy = realAttr("cy")
  val r = realAttr("r")

  case class EventListenerModifier(event: String, listener: js.Function1[_ <: Event, _]) extends Modifier
  object onClick { def :=(f: js.Function1[MouseEvent, _]): EventListenerModifier = EventListenerModifier("click", f) }

  trait VNodeElement {
    def elementName: String
    def ns: js.UndefOr[String] = js.undefined
    def apply(mods: Modifier*): VNode = {
      val children = js.Array[VNode | String]()
      val attrs = js.Dictionary[String]()
      val listeners = js.Dictionary[js.Function1[_ <: Event, _]]()
      for (mod <- mods) {
        mod match {
          case ChildVNodeModifier(child) => children.push(child)
          case ChildTextNodeModifier(text) => children.push(VNode.vnode(text = text))
          case ManyChildModifier(cs) =>
            for (child <- cs) children.push(child.child)
          case AttributeModifier(attr, value) => attrs.update(attr, value)
          case EventListenerModifier(event, listener) => listeners.update(event, listener)
        }
      }
      VNode.vnode(elementName, data = VNodeData(attrs = attrs, on = listeners, ns = ns), children = children)
    }
  }
  trait SVGVNodeElement extends VNodeElement {
    final override val ns: js.UndefOr[String] = "http://www.w3.org/2000/svg"
  }

  object div extends VNodeElement { val elementName = "div" }
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
