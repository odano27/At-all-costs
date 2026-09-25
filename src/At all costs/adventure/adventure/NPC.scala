package adventure

/** A non-player character that the player can `ask` for a favour.
  *
  * @param name  an identifier for the character (not shown to the player)
  * @param reply what happens when the player asks the character for something */
class NPC(val name: String, val reply: String):

  override def toString = this.name

end NPC
