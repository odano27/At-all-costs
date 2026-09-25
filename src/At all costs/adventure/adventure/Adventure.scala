package adventure

/** One play-through of “Leave at all costs”.
  *
  * The game has two phases:
  *  1. a multiple-choice phase, in which the player answers numbered choices, and
  *  2. an adventure phase, in which the player walks around the school looking for a way to
  *     unlock the exit (see the README for the full map).
  *
  * @param playerName       the player's name, or an empty string if it has not been asked yet
  * @param endingsSoFar     the endings reached in earlier play-throughs */
class Adventure(var playerName: String, endingsSoFar: Set[Ending] = Set()):

  /** The name of the game, with the player's name included. */
  def title = if this.playerName.isEmpty then "Leave at all costs" else s"Leave at all costs, ${this.playerName}"

  /** All the endings reached so far, including those of earlier play-throughs. */
  var reachedEndings: Set[Ending] = endingsSoFar

  def allEndingsReached = this.reachedEndings.size == Ending.values.length


  // ------------------------------------------------------------------------------------------
  //  Multiple-choice phase. Descriptions that mention the player's name are filled in by
  //  `writeStory` once the name is known.
  // ------------------------------------------------------------------------------------------

  private val attendance =
    val prompt = if this.playerName.isEmpty then "What's your name again?" else "Type anything to begin"
    Area("multi", s"You're at school. It's 10:00, and the Civics teacher is taking attendance.\n$prompt", false)
  private val rollCall      = Area("multi0", "", false)
  private val fellAsleep    = Area("multi02", "", false)
  private val nurseQuestion = Area("multi0111", "", false)
  private val planEscape    = Area("multi01", "You're in the Civics class.|You can't take it any longer, you must get out BY ALL MEANS\n\"Now what?\", you think to yourself, \"Do I...\"\n1: Fake being ill\n2: Sneak out while you \"go to the toilet\"", false)
  private val fakeIllness   = Area("multi011", "You're in the Civics class.|You fake a few coughs and sniffle your nose, quite convincingly so.\n\"Can I go to the infirmary?\", you ask the teacher, shakily.\n\"Yes…\", they're giving it some thought, \"I'll… have to escort you there though\"\n1: Let them accompany you\n2: Say it's okay", false)
  private val refusedEscort = Area("multi0112", "You're in the Civics class.|\"It's nothing really, I can go by myself\", your nervousness is showing.\n\"Yeah… right…\", they sound eerily menacing. \"How about… YOU go back to your seat and stop FAKING your sickness. Oh, and… remember to visit the detention room this afternoon.\"\nYou couldn't keep up the act and they saw right through you.", false)
  private val headacheLie   = Area("multi01111", "You're at the infirmary.|\"I... don't remember you mentioning a headache? And since when did your sniffling stop?\"\nYou couldn't even keep your story straight. The nurse lets out an audible sigh.\n\"Alright. This... theatrics ends now, you are wasting my time!\"\n\"I'm afraid I'll have to write you up. No, YOU should be afraid, young one.\"", false)


  // ------------------------------------------------------------------------------------------
  //  The school. Floor 1 (where the class is), floor 0 (where the exit is), the corner
  //  staircase up to the rooftop, and the areas that end the game.
  // ------------------------------------------------------------------------------------------

  private val hallCentre   = Area("Central", "You're at the central hallway.\nThe notice screen says: Our security called in sick today.\nContact the principal if you need to leave early.\n\nAsking them to help you sneak out isn't a good idea...", true)
  private val robotics     = Area("North", "You're at the robotics room.\nThere's a group of students working on a project in the corner, best not disturb them.", true)
  private val room13       = Area("South", "You're at the South hallway.\nThe room 1.3 is empty today.", true)
  private val room12       = Area("East", "You're at the East hallway.\nYour best friend should be studying in room 1.2 now.\nYou can go down stairs from here.\nYou really shouldn't go up, all classes are full up there.", true)
  private val classroom    = Area("West", "You're at the West hallway, right outside your class.|\"Excuse me, can I go to the toilet?\". This should be fine, people have sneaked out like this before.\n\"Go\", she doesn't even give you a look and continues on with her boring material.", true)
  private val cornerStairs = Area("Stairs", "You're at the stairs in the South West corner.\nYou really shouldn't go up, but nobody should notice you in this corner...", true)

  private val groundCentre = Area("Ground Central", "You're at the middle of the school grounds.\nThe notice screen says: Our security called in sick today.\nContact the principal if you need to leave early.\nYou can see the exit to the South East, across the yard.", true)
  private val library      = Area("Ground North", "You're at the library in the north.\nA pyromania thought lights up, whatever that means...", true)
  private val chemLab      = Area("Ground South", "South of the school grounds, the Chemistry lab.\nIt's empty, maybe you can grab something real quick", true)
  private val infirmary    = Area("Ground East", "East of the school grounds, You're at the infirmary.\nToday's nurse really IS your favourite.|\"You can go back to teaching now.\", the nurse addresses your teacher.\n\"Now, now. I know what's going on...\", you can hear your heart pounding.\n\"But, and this BETTER be a secret, here's a sick note for you, dear.\"", true)
  private val physicsLab   = Area("Ground West", "West of the school grounds, the Physics lab.\nThe lab instructor is inside. You can ask them for a tool.", true)
  private val principals   = Area("Ground Stairs", "You're at the stairs in the South West corner.\nThe only way is up! Well, there's the principal's office over there... you're going to need a good reason before you ask them anything.", true)
  private val exit         = Area("Exit", "The exit... it's locked with a padlock.\nYou've got to get out of here. Maybe you can pry the padlock open with some tools?", true)

  private val stairs2      = Area("2 Stairs", "Well, you can always go up...", true)
  private val stairs3      = Area("3 Stairs", "Might as well go to the rooftop, no?", true)
  private val stairs4      = Area("4 Stairs", "You really should've taken more of that cardio...", true)
  private val stairs5      = Area("5 Stairs", "Almost there... C'mon you", true)
  private val rooftop      = Area("Rooftop", "Why\nare\nyou\nhere?\nAnd why is that railing to your North broken?", true)
  private val rooftopEdge  = Area("Rooftop North", "This is a nice view of the city. It's really easy to jump down from here, someone should fix this!", true)

  private val jumped       = Area("WHY", "There are other ways out.\nCall 0925250113 (Finnish, English, Swedish, Arabic) if you or someone you know is in a crisis and you need assistance.", false)
  private val street       = Area("OUT", "Freedom.\nAt last", false)
  private val hospital     = Area("Hospital", "You wake up in the hospital, cuffed to the sick bed.", false)

  /** Areas that end the game badly as soon as the player enters them, with their endings. */
  private val losingAreas = Map(
    this.fellAsleep    -> Ending.FellAsleep,
    this.refusedEscort -> Ending.Humiliated,
    this.headacheLie   -> Ending.CaughtLying,
    this.jumped        -> Ending.Jumped
  )

  this.buildSchool()

  private def buildSchool(): Unit =
    // Floor 1
    hallCentre  .setNeighbors(Vector("north" -> robotics, "east" -> room12, "south" -> room13, "west" -> classroom))
    robotics    .setNeighbors(Vector("south" -> hallCentre))
    room13      .setNeighbors(Vector("north" -> hallCentre, "west" -> cornerStairs))
    room12      .setNeighbors(Vector("west" -> hallCentre, "down" -> infirmary))
    classroom   .setNeighbors(Vector("east" -> hallCentre, "south" -> cornerStairs))
    cornerStairs.setNeighbors(Vector("north" -> classroom, "east" -> room13, "up" -> stairs2, "down" -> principals))
    // Floor 0
    groundCentre.setNeighbors(Vector("north" -> library, "east" -> infirmary, "south" -> chemLab, "west" -> physicsLab, "southeast" -> exit))
    library     .setNeighbors(Vector("south" -> groundCentre))
    chemLab     .setNeighbors(Vector("north" -> groundCentre, "east" -> exit, "west" -> principals, "northeast" -> infirmary))
    infirmary   .setNeighbors(Vector("south" -> exit, "west" -> groundCentre, "up" -> room12, "southwest" -> chemLab))
    physicsLab  .setNeighbors(Vector("east" -> groundCentre, "south" -> principals))
    principals  .setNeighbors(Vector("north" -> physicsLab, "east" -> chemLab, "up" -> cornerStairs))
    exit        .setNeighbors(Vector("north" -> infirmary, "west" -> chemLab, "northwest" -> groundCentre))
    // Up to the rooftop
    stairs2     .setNeighbors(Vector("up" -> stairs3, "down" -> cornerStairs))
    stairs3     .setNeighbors(Vector("up" -> stairs4, "down" -> stairs2))
    stairs4     .setNeighbors(Vector("up" -> stairs5, "down" -> stairs3))
    stairs5     .setNeighbors(Vector("up" -> rooftop, "down" -> stairs4))
    rooftop     .setNeighbors(Vector("north" -> rooftopEdge, "down" -> stairs5))
    rooftopEdge .setNeighbors(Vector("south" -> rooftop))
    rooftopEdge .setHiddenNeighbor("down", jumped)

    robotics.addItem(Item("motor", "A strong motor.\nYou've heard it's even strong enough to bend steel."))
    chemLab .addItem(Item("black bottle", "A black heavy bottle. The label says:\n\nH2SO4 96% (Ultrapur)\n      Danger\n    H290 - H314\n"))
    stairs2 .addItem(Item("red canister", "A large red canister.\nIt reeks of some kind of fuel.\nWhy did you find it there?"))

    infirmary .addNPC(NPC("nurse", "You ask them for a sick note.\n\"It's Civics, isn't it...\"\n\"Here you go.\"\nYou get the sick note."))
    physicsLab.addNPC(NPC("instructor", "You ask them for an L-shaped wrench.\n\"Remember to return it!\"\nYou get the wrench."))
    principals.addNPC(NPC("principal", "You ask them for a sick leave.\nThe principal checks the note for a while.\n\"I'll go unlock the exit.\""))

  /** Fills in the multiple-choice story once the player's name is known. */
  private def writeStory(): Unit =
    rollCall.description      = s"You're in class.|\"${playerName}\", they called out. You reluctantly raise your hand.\nIt's only 10:05, there's 85 minutes of boredom ahead.\n1: Sneak out\n2: It's probably fine"
    fellAsleep.description    = s"You're in class.|It's 10:30, you've been sleeping.\n\"${playerName}\", the teacher yelled, \"Enough is enough, detention!\".\nThis is the third time they caught you sleeping this semester..."
    nurseQuestion.description = s"You're at the infirmary.|You went to the infirmary with your teacher.\n\"${playerName}, is it? How are you feeling, dear?\", it's your favourite nurse. What a relief!\n1: \"My head. It really hurts!\"\n2: \"It's really hard to breathe!\", *sniffle*, \"My throat is really sore too.\""
    attendance   .setChoices(Vector(0 -> rollCall))
    rollCall     .setChoices(Vector(1 -> planEscape, 2 -> fellAsleep))
    planEscape   .setChoices(Vector(1 -> fakeIllness, 2 -> classroom))
    fakeIllness  .setChoices(Vector(1 -> nurseQuestion, 2 -> refusedEscort))
    nurseQuestion.setChoices(Vector(1 -> headacheLie, 2 -> infirmary))


  /** The character that the player controls in the game. */
  val player = Player(this.attendance)


  // ------------------------------------------------------------------------------------------
  //  Counters and limits
  // ------------------------------------------------------------------------------------------

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 40

  /** How many turns the player may spend outside their class before the teacher notices.
    * Only limited if the player faked being ill: when they "went to the toilet", nobody is watching. */
  private var teacherLimit: Option[Int] = None
  private var turnsOutsideClass = 0

  /** How many turns the player can spend in the burning library before passing out. */
  private val smokeLimit = 2
  private var turnsInSmoke = 0

  /** Whether the exit has been opened in the game world (see `openExit`). */
  private var exitOpen = false

  /** Returns true if the player is in a physical area, i.e. the adventure phase has started. */
  def started = this.player.location.physical

  private def teacherNoticed = this.teacherLimit.exists(this.turnsOutsideClass >= _)
  private def passedOut      = this.turnsInSmoke >= this.smokeLimit
  private def outOfTime      = this.turnCount >= this.timeLimit
  private def libraryOnFire  = this.player.unlockMethod.contains(UnlockMethod.Burned)


  // ------------------------------------------------------------------------------------------
  //  Changes to the world
  // ------------------------------------------------------------------------------------------

  /** Called when the player leaves the class by asking to go to the toilet. */
  private def startFromToilet(): Unit =
    infirmary.description = "You're at the infirmary.\nToday's nurse is your favourite, you might be able to ask them for a favor."

  /** Called when the player leaves the class by convincing the nurse that they are ill. */
  private def startFromInfirmary(): Unit =
    classroom.description = "You're at the West hallway, right outside your class.\nThe Civics teacher could notice you any moment!"
    this.player.addItem(Item("sick note", "A detailed sick note written for you by the lovely nurse."))
    this.teacherLimit = Some(3)
    infirmary.addNPC(NPC("nurse0", "\"Give the note to the principal, dear.\""))

  /** Opens the exit after the player has unlocked it one way or another. */
  private def openExit(): Unit =
    exit.description = "You're at the exit.\nIt's not locked anymore, you're free to leave."
    exit.setNeighbor("out", street)
    if this.libraryOnFire then
      library.description = "You're at the library.\nThere's a large fire in the back. Who started it???"
    this.exitOpen = true


  // ------------------------------------------------------------------------------------------
  //  Commands that change the world
  // ------------------------------------------------------------------------------------------

  private val pryText  = "You create a leverage system by using the wrench with the motor and easily pry the lock open."
  private val burnText = "You empty the fuel in the canister onto the floor.\nYou grab a book and pour some acid on to heat it up then throw it on the ground.\nA large fire breaks out, the alarm goes off. Luckily there wasn't anyone in there...\nThe exit should be unlocked now, not that you need to go back to class..."
  private val alreadyBurningText = "The alarms are going off... Somebody already lit this place on fire."
  private val alreadyOpenText    = "The lock is already down. You're not gonna bring the school down too right?\nRight?"

  /** `use <item>`: items only do something at the exit or in the library. */
  private def use(itemName: String): String =
    if !this.player.has(itemName) then "You don't have that."
    else if this.player.location == exit then this.useAtExit(itemName)
    else if this.player.location == library then this.useInLibrary(itemName)
    else "Use it on... what?"

  private def useAtExit(itemName: String): String = itemName match
    case "motor" | "wrench" =>
      if this.player.hasUnlockedExit then "You can't use that. It's already unlocked."
      else if this.player.canPry then this.pry()
      else "You need something else to use that with!"
    case "black bottle" =>
      if this.player.hasUnlockedExit then "You can't use that here."
      else
        this.player.unlockWith(UnlockMethod.Dissolved)
        "You slowly pour the content of the bottle onto the padlock's shackle as it dissolves.\nIt took some time, but the lock drops to the ground, shackle-less.\nHow did none of the acid get on you???"
    case _ => "You can't use that here."

  private def useInLibrary(itemName: String): String = itemName match
    case "red canister" | "black bottle" =>
      if this.player.canBurn then this.burn()
      else "You need something else to use that with!"
    case _ => "You can't use that here."

  /** `pry`: pry the padlock open with the motor and the wrench. */
  private def pry(): String =
    if this.player.location != exit then "Pry what?"
    else if this.player.hasUnlockedExit then "Pry what? It's already unlocked."
    else if !this.player.canPry then "You don't have the tools to do that."
    else
      this.player.unlockWith(UnlockMethod.Pried)
      pryText

  /** `burn`: set the library on fire, which sets off the alarm and unlocks the exit. */
  private def burn(): String =
    if this.player.location != library then "Burn what?"
    else if !this.player.canBurn then "You don't have enough materials to start a fire."
    else if this.libraryOnFire then alreadyBurningText
    else if this.player.hasUnlockedExit then alreadyOpenText
    else
      this.player.unlockWith(UnlockMethod.Burned)
      burnText

  /** `jump`: only possible where the railing is broken. */
  private def jump(): String =
    if this.player.location != rooftopEdge then "Jump where?"
    else
      this.player.goTo(jumped)
      "..."


  // ------------------------------------------------------------------------------------------
  //  Playing turns
  // ------------------------------------------------------------------------------------------

  /** Plays a turn by executing the given in-game command, such as “go west”. Returns a textual
    * report of what happened, or an error message if the command was unknown. Unknown commands,
    * “help” and “endings” do not use up a turn. */
  def playTurn(command: String): String =
    val action = Action(command)
    val report =
      if this.isOver then "The game is over."
      else action.verb match
        case "help"    => this.help
        case "endings" => this.endingsReport
        case _ if !this.started => this.playChoiceTurn(action)
        case _ => this.playAdventureTurn(action)
    this.recordEnding()
    report

  /** A turn in the multiple-choice phase. The first answer is the player's name (if not known yet);
    * after that the player answers "1" or "2". */
  private def playChoiceTurn(action: Action): String =
    if this.turnCount == 0 then
      if this.playerName.isEmpty then
        this.playerName = action.words.map(_.capitalize).mkString(" ")
      this.writeStory()
      this.player.choose(0)
    else action.verb match
      case "1" | "2" => this.player.choose(action.verb.toInt)
      case "quit"    => return this.player.quit()
      case _         => return "𝗪𝗿𝗶𝘁𝗲 𝗼𝗻𝗹𝘆 \"𝟭\" 𝗼𝗿 \"𝟮\"\n" + this.player.location.eventText
    this.turnCount += 1
    if this.player.location == classroom then this.startFromToilet()
    if this.player.location == infirmary then this.startFromInfirmary()
    this.player.location.eventText

  /** A turn in the adventure phase. */
  private def playAdventureTurn(action: Action): String =
    val wasOnFire = this.libraryOnFire
    val outcome = action.verb match
      case "use"  => Some(this.use(action.modifiers))
      case "pry"  => Some(this.pry())
      case "burn" => Some(this.burn())
      case "jump" => Some(this.jump())
      case _      => action.execute(this.player)
    outcome match
      case None => s"Unknown command: \"${action.input.trim}\"."
      case Some(report) =>
        this.turnCount += 1
        if this.player.location == classroom then this.turnsOutsideClass += 1
        if wasOnFire && this.player.location == library then this.turnsInSmoke += 1
        if this.player.hasUnlockedExit && !this.exitOpen then this.openExit()
        report

  private def help: String =
    if this.turnCount == 0 then
      "Choose your name.\nYou'll have 2 choices for each turn, write \"1\" or \"2\" corresponding to your choices.\nYou can also see endings you've already reached by keying in \"endings\".\nYou may ask for further instructions if you pass the initial multiple choice stage.\nIf you don't have a name yet, you must type a name of your choice. Anything but \"help\"..."
    else if !this.started then
      s"${this.player.location.eventText}\n\nWrite \"1\" or \"2\" corresponding to your choice.\nYou can also see endings you've already reached by keying in \"endings\"."
    else
      "You can access the map through the \"Program\" menu (opens external map file).\n\"go \" + {direction}: go to specified direction\n\"get \" + {item name}: pick up specified items that you might come by\n\"drop \" + {item name}: drop an item you are carrying\n\"examine \" + {item name}: take closer look at the item\n\"use \" + {item name}: use specified item\n\"inventory\": see what items are available\n\"ask\": interact with people\n\"endings\": see list of endings reached\nThere are some other commands for you to find out, hinted in the location description ^^!"

  private def endingsReport: String =
    val list =
      if this.reachedEndings.isEmpty then "No endings reached."
      else "Endings reached: " + Ending.values.filter(this.reachedEndings.contains).mkString("\"", "\", \"", "\"")
    if this.started then list else (this.player.location.eventText + "\n\n" + list).trim


  // ------------------------------------------------------------------------------------------
  //  The end of the game
  // ------------------------------------------------------------------------------------------

  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.street

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver =
    this.isComplete || this.player.hasQuit || this.outOfTime || this.losingAreas.contains(this.player.location) ||
      this.teacherNoticed || this.passedOut

  /** The ending that the player has reached, if any. Running out of time or quitting does not count. */
  def ending: Option[Ending] =
    if this.isComplete then this.player.unlockMethod.map(_.ending)
    else if this.outOfTime || this.player.hasQuit then None
    else if this.teacherNoticed then Some(Ending.TeacherNoticed)
    else if this.passedOut then Some(Ending.PassedOut)
    else this.losingAreas.get(this.player.location)

  /** Records the ending once the game is over. Passing out in the smoke also lands you in hospital. */
  private def recordEnding(): Unit =
    if this.isOver then
      this.reachedEndings ++= this.ending
      if this.passedOut then this.player.goTo(this.hospital)

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "(One of the endings based on true story)\nThis is a very short text-based game with multiple endings, controlled using text inputs. The main goal is to reach all endings.\nFeel free to use the provided map later on (in the menu), but first you have some choices to make.\nType \"help\" for further instructions."

  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on the player's actions. */
  def goodbyeMessage: String =
    val story =
      if this.isComplete then this.ending.map(_.title).getOrElse("")
      else if this.outOfTime then
        "Oh no! The principal stands behind you. They noticed you wandering around during class!\nGame over! You should've been quicker!\nYou will not unlock any new ending."
      else if this.player.hasQuit then ""
      else this.ending match
        case Some(Ending.TeacherNoticed) => "The Civics teacher noticed. They barge out towards you.\nYou're not gonna be able to lie to them again. Game over!"
        case Some(Ending.PassedOut)      => "You pass out due to inhaling too much smoke. Luckily you were rescued by the nurse.\nThe police got your fingerprints from a fuel can they found...\nFool around and find out!"
        case Some(Ending.FellAsleep)     => "Zzz...Zzz... Game over!"
        case Some(Ending.Humiliated)     => "Humiliated in front of the class! Game over!"
        case Some(Ending.CaughtLying)    => "Stick to your lies! Game over!"
        case Some(Ending.Jumped)         => "Help is always available at Aalto University"
        case _                           => ""
    val retryHint =
      if this.allEndingsReached then ""
      else "\n\n𝐘𝐨𝐮 𝐜𝐚𝐧 𝐭𝐫𝐲 𝐚𝐠𝐚𝐢𝐧 𝐟𝐨𝐫 𝐦𝐨𝐫𝐞 𝐞𝐧𝐝𝐢𝐧𝐠𝐬. The option is available in the \"Program\" menu."
    story + retryHint

end Adventure
