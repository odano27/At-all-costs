package adventure

/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect,
  * parsers for such commands. An action object is immutable after creation.
  *
  * The command is split into a verb (its first word) and modifiers (the rest). Case and
  * extra whitespace are ignored, so “Get   Black Bottle” means the same as “get black bottle”.
  *
  * @param input a textual in-game command such as “go east” or “rest” */
class Action(val input: String):

  /** The words of the command, in lower case. */
  val words = input.trim.toLowerCase.split("\\s+").toVector.filter(_.nonEmpty)

  /** The first word of the command, such as “go” in “go east”. */
  val verb = this.words.headOption.getOrElse("")

  /** The rest of the command, such as “east” in “go east”. */
  val modifiers = this.words.drop(1).mkString(" ")

  /** Causes the given player to take the action represented by this object, assuming
    * that the command was understood. Returns a description of what happened as a result
    * of the action (such as “You go west.”). The description is returned in an `Option`
    * wrapper; if the command was not recognized, `None` is returned.
    *
    * Only commands that concern the player alone are handled here; commands that change the
    * game world (such as “use” or “burn”) are handled by [[Adventure]]. */
  def execute(actor: Player): Option[String] = this.verb match
    case "go"        => Some(actor.go(this.modifiers))
    case "rest"      => Some(actor.rest())
    case "inventory" => Some(actor.inventory)
    case "drop"      => Some(actor.drop(this.modifiers))
    case "examine"   => Some(actor.examine(this.modifiers))
    case "get"       => Some(actor.get(this.modifiers))
    case "ask"       => Some(actor.ask())
    case "quit"      => Some(actor.quit())
    case _           => None

  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = s"$verb (modifiers: $modifiers)"

end Action
