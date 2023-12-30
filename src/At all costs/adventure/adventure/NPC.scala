package adventure

/** This can be treated same as the item class, but fewer methods will be using this */
class NPC(val name: String, var description: String):

  def altdes(newdes: String) =
    description = newdes

  override def toString = this.name

end NPC
