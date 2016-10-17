package game.items

import scala.collection.mutable
import scala.reflect.ClassTag

class Item private () {
  private val components = mutable.Buffer.empty[ItemComponent]

  def component[T: ClassTag] = components.collectFirst {
    case t: T => t
  }

  def apply[T: ClassTag] = component[T].get
}

object Item {
  def fromDefinition(itype: ItemDef): Item = {
    val item = new Item
    for (c <- itype.componentProps) {
      item.components.append(c.instantiate)
    }
    item
  }
}
