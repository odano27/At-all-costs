package adventure

/** Every ending the player can collect. The goal of the game is to reach all of them.
  *
  * @param title the name of the ending, as shown to the player */
enum Ending(val title: String):
  // Bad endings of the multiple-choice phase
  case FellAsleep       extends Ending("Zzz...Zzz")
  case Humiliated       extends Ending("Humiliating!")
  case CaughtLying      extends Ending("Stick to your lies!")
  // Good endings: one for each way of unlocking the exit
  case SickNote         extends Ending("Thank god for the nurse.")
  case Pried            extends Ending("Lucky it ran on battery!")
  case Dissolved        extends Ending("Dexterity 100!")
  case Burned           extends Ending("Certified pyromaniac")
  // Bad endings of the adventure phase
  case Jumped           extends Ending("A-Are you okay?")
  case TeacherNoticed   extends Ending("You already lied once...")
  case PassedOut        extends Ending("Fool around and find out!")

  override def toString = this.title

end Ending


/** The ways in which the locked exit can be opened. Each one leads to a different good ending. */
enum UnlockMethod(val ending: Ending):
  case Principal extends UnlockMethod(Ending.SickNote)
  case Pried     extends UnlockMethod(Ending.Pried)
  case Dissolved extends UnlockMethod(Ending.Dissolved)
  case Burned    extends UnlockMethod(Ending.Burned)

end UnlockMethod
