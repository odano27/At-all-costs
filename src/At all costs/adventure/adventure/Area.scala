package adventure

import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an “area” can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  *
  * @param name        the name of the area
  * @param description a basic description of the area (typically not including information about items) */
class Area(var name: String, var description: String, val physical: Boolean):

  val neighbors = Map[String, Area]()
  private val choices = Map[Int, Area]()
  private var items = Vector[Item]()
  var NPC = Vector[NPC]()

  def setChoices(c: Vector[(Int, Area)]): Unit =
    this.choices ++= c

  def choice(n: Int) = this.choices.get(n)

  def addNPC(name: NPC) =
    NPC = NPC.appended(name)

  def addItem(item: Item) =
    items = items.appended(item)

  def removeItem(n: String): Option[Item] =
    if !this.contains(n) then None
    else
      val a = items(items.indexWhere(_.name == n))
      items = items.filterNot(_ == a)
      Option(a)

  def contains(n: String) = items.map(_.name).contains(n)

  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)

  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) =
    this.neighbors += direction -> neighbor

  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction–area pairs.
    *
    * @param exits contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) =
    this.neighbors ++= exits


  /** Returns a multi-line description of the area as a player sees it. */
  def fullDescription =
    val a = if items.nonEmpty then s"\n\nYou see here: ${items.mkString(", ")}" else ""
    val b = this.description.takeWhile(_ != '|')
    val exitList =
      if this.physical then
        if this.name == "Rooftop North" then "\n\nYou can go: south" else "\n\nYou can go: " + this.neighbors.keys.mkString(", ")
      else ""

    b + a + exitList


  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)

end Area

