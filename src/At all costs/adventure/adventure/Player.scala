package adventure

import scala.collection.mutable.Map

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea the player’s initial location */
class Player(startingArea: Area):

  private var currentLocation = startingArea // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false // one-way flag
  private var pItems = Map[String, Item]()
  var name = ""
  var unlocked = false //one-way flag
  var unlockMethod = ""

  def canDissolve = pItems.contains("black bottle") //the player can dissovle the lock away...

  def canPry = pItems.contains("wrench") && pItems.contains("motor") //the player can pry the lock open

  def canBurn = pItems.contains("black bottle") && pItems.contains("red canister") //the player can burn down the library

  def unlock() =
    unlocked = true

  def pry() = //change params within this class
    unlockMethod = "pried"
    unlock()

  def burn() =
    unlockMethod = "burned"
    unlock()

  def dissolve() =
    unlockMethod = "dissolved"
    unlock()

  def choose(n: Int): Unit = //use to implement multiple choice by moving to next choice "Area"
    val destination = this.location.choice(n)
    this.currentLocation = destination.getOrElse(this.currentLocation)

  def goTo(a: Area): Unit = //go to certain Area, useful for updating areas.
    currentLocation = a

  def addItem(a: Item) =
    pItems += (a.name -> a)

  def name(s: String): Unit =
    name = s

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  /** Returns the player’s current location. */
  def location = this.currentLocation

  /** A dark twist */
  def jump() =
    if this.location.name == "Rooftop North" then
      this.currentLocation = this.location.neighbor("down").getOrElse(this.currentLocation)
      "..."
    else "Jump where?"

  def help() =
    if this.location.name.contains("multi") then
      if this.location.name == "multi" then s"""Choose your name.\nYou'll have 2 choices for each turn, write "1" or "2" corresponding to your choices.\nYou can also see endings you've already reached by keying in "endings".\nYou may ask for further instructions if you pass the initial multiple choice stage.\nIf you don't have a name yet, you must type a name of your choice. Anything but "help"..."""
      else s"""${location.description.dropWhile(_ != '|').drop(1)}\n\nWrite "1" or "2" corresponding to your choice.\nYou can also see endings you've already reached by keying in "endings"."""
    else
      s"""You can access the map through the "Program" menu (opens external map file).\n"go " + {direction}: go to specified direction\n"get " + {item name}: pick up specified items that you might come by\n"examine " + {item name}: take closer look at the item\n"use " + {item name}: use specified item\n"inventory": see what items are available\n"ask": interact with people\n"endings": see list of endings reached\nThere are some other commands for you to find out, hinted in the location description ^^!"""


  def ask() = //ask people for a favor, if there is any at the player's location
    if this.location.NPC.nonEmpty then
      var a = ""
      val b = this.location.NPC(this.location.NPC.size - 1)
      a = b.name match
        case "nurse" =>
          addItem(Item("sick note", "a detailed sick note written for you by the lovely nurse"))
          this.location.addNPC(NPC("nurse0", "\"Give the note to the principal, dear.\""))
          s"${b.description}"
        case "instructor" =>
          addItem(Item("wrench", "A really strong 1 inch L-shaped wrench. If only you're strong enough to pry the lock with it..."))
          this.location.addNPC(NPC("instructor0", "\"Remember to return the wrench!\""))
          s"${b.description}"
        case "principal" =>
          if has("sick note") then
            this.location.addNPC(NPC("principal0", "\"The door is unlocked, you can leave through the exit!\""))
            unlock()
            unlockMethod = "principal"

            s"${b.description}"
          else "You don't have a reason to ask for a leave, do you?"
        case other => s"${b.description}"
      a
    else "Ask who?"

  def get(n: String): String =
    if currentLocation.contains(n) then
      val a = currentLocation.removeItem(n).get
      pItems += (n -> a)
      s"You pick up the ${n}."
    else s"There is no ${n} here to pick up."

  def drop(n: String): String =
    if !has(n) then "You don't have that!"
    else
      val a = pItems(n)
      currentLocation.addItem(a)
      pItems -= n
      s"You drop the ${n}."

  def examine(n: String): String =
    if !has(n) then "If you want to examine something, you need to pick it up first."
    else s"You look closely at the ${n}.\n${pItems(n).description}"

  def has(n: String) = pItems.contains(n)

  def inventory: String =
    if pItems.isEmpty then "You are empty-handed."
    else s"You are carrying:\n${pItems.keys.mkString("\n")}"

  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player’s current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */

  def go(direction: String) =
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if destination.isDefined then "You go " + direction + "." else "You can't go " + direction + "."


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() =
    "You rest for a while. Better get a move on, though."


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() =
    this.quitCommandGiven = true
    "You quit."

  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

end Player

