package game.items

import scala.collection.mutable

trait ItemComponent

abstract class ComponentProps[Props, Component <: ItemComponent](build: Props => Component) { self: Props =>
  def instantiate: Component = build(this)
}

case class WearableProperties(
  location: String,
  encumbrance: Int,
  coverage: Double
) extends ComponentProps[WearableProperties, Wearable](new Wearable(_))

class Wearable(val props: WearableProperties) extends ItemComponent

case class ContainerProperties(
  maxVolume: String
) extends ComponentProps[ContainerProperties, Container](new Container(_))

class Container(val props: ContainerProperties) extends ItemComponent {
  val containedItems = mutable.Buffer.empty[Item]

  /*def findRecursive(f: Item => Boolean): Seq[Item] = {
    containedItems.filter(f) ++ containedItems.flatMap { c => c.component[Container].flatMap(_.findRecursive(f)) }
  }*/
}


case class GunProperties(
  maxAmmo: Int
) extends ComponentProps[GunProperties, Gun](new Gun(_))

class Gun(val props: GunProperties) extends ItemComponent {
  var ammo = props.maxAmmo
}


case class AmmoProperties(
  ammoType: String
) extends ComponentProps[AmmoProperties, Ammo](new Ammo(_))

class Ammo(val props: AmmoProperties) extends ItemComponent

case class ItemDef(
  id: String,
  name: String,
  componentProps: Seq[ComponentProps[_, _ <: ItemComponent]]
)
