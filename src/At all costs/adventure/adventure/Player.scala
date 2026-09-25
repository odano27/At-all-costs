package adventure

import scala.collection.mutable

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea the player’s initial location */
class Player(startingArea: Area):

  private var currentLocation = startingArea
  private var quitCommandGiven = false             // one-way flag
  private val items = mutable.Map[String, Item]()
  private var exitUnlockedBy: Option[UnlockMethod] = None

  /** Returns the player’s current location. */
  def location = this.currentLocation

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  // --- Unlocking the exit ---

  /** How the player got the exit open, or `None` if it is still locked. */
  def unlockMethod = this.exitUnlockedBy

  def hasUnlockedExit = this.exitUnlockedBy.isDefined

  /** Records that the exit was opened with the given method. Only the first method counts. */
  def unlockWith(method: UnlockMethod): Unit =
    if !this.hasUnlockedExit then
      this.exitUnlockedBy = Some(method)

  /** The motor and the wrench together make a lever strong enough for the padlock. */
  def canPry = this.has("wrench") && this.has("motor")

  /** Fuel and acid (to heat up the fuel) are needed to start a fire. */
  def canBurn = this.has("black bottle") && this.has("red canister")

  // --- Moving around ---

  /** Moves the player along the given option of a multiple-choice step. An unknown option
    * leaves the player where they are. */
  def choose(option: Int): Unit =
    this.currentLocation = this.location.choice(option).getOrElse(this.currentLocation)

  /** Moves the player directly to the given area. */
  def goTo(area: Area): Unit =
    this.currentLocation = area

  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player’s current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) =
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if destination.isDefined then s"You go $direction." else s"You can't go $direction."

  // --- People ---

  /** Asks the person at the player’s location for a favour, if there is anyone there. */
  def ask(): String =
    this.location.npc match
      case None => "Ask who?"
      case Some(person) => person.name match
        case "nurse" =>
          this.addItem(Item("sick note", "A detailed sick note written for you by the lovely nurse."))
          this.location.addNPC(NPC("nurse0", "\"Give the note to the principal, dear.\""))
          person.reply
        case "instructor" =>
          this.addItem(Item("wrench", "A really strong 1 inch L-shaped wrench. If only you're strong enough to pry the lock with it..."))
          this.location.addNPC(NPC("instructor0", "\"Remember to return the wrench!\""))
          person.reply
        case "principal" =>
          if this.hasUnlockedExit then
            "The principal gives you a suspicious look.\n\"Somebody already opened the exit... Do you know anything about that?\""
          else if this.has("sick note") then
            this.location.addNPC(NPC("principal0", "\"The door is unlocked, you can leave through the exit!\""))
            this.unlockWith(UnlockMethod.Principal)
            person.reply
          else "You don't have a reason to ask for a leave, do you?"
        case _ => person.reply

  // --- Items ---

  def has(itemName: String) = this.items.contains(itemName)

  def addItem(item: Item): Unit =
    this.items += item.name -> item

  def get(itemName: String): String =
    this.location.removeItem(itemName) match
      case Some(item) =>
        this.addItem(item)
        s"You pick up the $itemName."
      case None => s"There is no $itemName here to pick up."

  def drop(itemName: String): String =
    this.items.remove(itemName) match
      case Some(item) =>
        this.location.addItem(item)
        s"You drop the $itemName."
      case None => "You don't have that!"

  def examine(itemName: String): String =
    this.items.get(itemName) match
      case Some(item) => s"You look closely at the $itemName.\n${item.description}"
      case None => "If you want to examine something, you need to pick it up first."

  def inventory: String =
    if this.items.isEmpty then "You are empty-handed."
    else s"You are carrying:\n${this.items.keys.mkString("\n")}"

  // --- Other ---

  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() =
    "You rest for a while. Better get a move on, though."

  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result. */
  def quit() =
    this.quitCommandGiven = true
    "You quit."

  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

end Player
