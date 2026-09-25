package adventure

import scala.collection.mutable

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. What different areas have in common is that players can be located
  * in them and that they can have exits leading to other, neighboring areas.
  *
  * The game has two kinds of areas:
  *  - ''physical'' areas are places in the school that the player walks between;
  *  - non-physical areas are the steps of the opening multiple-choice story, where the
  *    player moves by picking a numbered choice instead of a direction.
  *
  * A description may consist of two parts separated by `|`: the text before the bar describes
  * the place itself and is shown in the "Location" panel; the text after it describes what just
  * happened when the player arrived and is shown in the "Events" panel.
  *
  * @param name        the name of the area
  * @param description a description of the area (not including information about items)
  * @param physical    whether this is a place in the school rather than a multiple-choice step */
class Area(val name: String, var description: String, val physical: Boolean):

  private val neighbors = mutable.LinkedHashMap[String, Area]()
  private val hiddenExits = mutable.Set[String]()
  private val choices = mutable.Map[Int, Area]()
  private var items = Vector[Item]()
  private var people = Vector[NPC]()

  /** The part of the description that describes the place itself. */
  def placeText = this.description.takeWhile(_ != '|')

  /** The part of the description that describes what happened on arrival (may be empty). */
  def eventText = this.description.dropWhile(_ != '|').drop(1)

  // --- Multiple-choice steps ---

  def setChoices(options: Vector[(Int, Area)]): Unit =
    this.choices ++= options

  /** Returns the area that the given multiple-choice option leads to, if there is such an option. */
  def choice(n: Int) = this.choices.get(n)

  // --- People ---

  /** Adds a character to this area. A character added later replaces the earlier ones as the
    * one the player talks to; this is used to change what a character says after a first visit. */
  def addNPC(person: NPC): Unit =
    this.people = this.people :+ person

  /** The character the player interacts with in this area, if any. */
  def npc: Option[NPC] = this.people.lastOption

  // --- Items ---

  def addItem(item: Item): Unit =
    this.items = this.items :+ item

  def removeItem(itemName: String): Option[Item] =
    val removed = this.items.find(_.name == itemName)
    removed.foreach( item => this.items = this.items.filterNot(_ eq item) )
    removed

  def contains(itemName: String) = this.items.exists(_.name == itemName)

  // --- Exits ---

  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)

  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area): Unit =
    this.neighbors += direction -> neighbor

  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction–area pairs. */
  def setNeighbors(exits: Vector[(String, Area)]): Unit =
    this.neighbors ++= exits

  /** Adds an exit that works but is not listed among the exits shown to the player. */
  def setHiddenNeighbor(direction: String, neighbor: Area): Unit =
    this.setNeighbor(direction, neighbor)
    this.hiddenExits += direction

  /** Returns a multi-line description of the area as a player sees it. */
  def fullDescription =
    val itemList = if this.items.nonEmpty then s"\n\nYou see here: ${this.items.mkString(", ")}" else ""
    val visibleExits = this.neighbors.keys.filterNot(this.hiddenExits.contains)
    val exitList = if this.physical then "\n\nYou can go: " + visibleExits.mkString(", ") else ""
    this.placeText + itemList + exitList

  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)

end Area
