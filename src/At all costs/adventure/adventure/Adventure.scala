package adventure

class Adventure(var pName: String):

  /** the name of the game, with the player's name included */
  def title =
    val x =
      if pName != "" then
        s", ${pName}"
      else  ""
    "Leave at all costs" + x

  /** list of endings */
  var endings = Map[String, Boolean](
    "Zzz...Zzz" -> false,
    "Humiliating!" -> false,
    "Stick to your lies!" -> false,
    "Thank god for the nurse." -> false,
    "Lucky it ran on battery!" -> false,
    "Dexterity 100!" -> false,
    "Certified pyromaniac" -> false,
    "A-Are you okay?" -> false,
    "You already lied once..." -> false,
    "Fool around and find out!" -> false
  )
  /** used to update endings list between this class and the swing application */
  def endingsReached(a: Map[String, Boolean]) =
    endings = a

  /** "Areas" representing the initial multiple choice phase */
  private val multi = if pName.isEmpty then Area("multi", s"""You're at school. It's 10:00, and the Civics teacher is taking attendance.\nWhat's your name again?""", false) else Area("multi", s"""You're at school. It's 10:00, and the Civics teacher is taking attendance.\n\nType anything to begin""", false)
  private var multi0 = Area("multi0", "", false)
  private var multi02 = Area("multi02", "", false)
  private val multi01 = Area("multi01", s"""You're in the Civics class.|You can't take it any longer, you must get out BY ALL MEANS\n"Now what?", you think to yourself, "Do I..."\n1: Fake being ill\n2: Sneak out while you "go to the toilet"""", false)
  private val multi011 = Area("multi011", s"""You're in the Civics class.|You fake a few cough, sniffle your nose, quite convincingly so.\n"Can I go to the infirmary?", you ask the teacher, shakingly.\n"Yes…", they're giving it some thought, "I'll… have to escort you there though"\n1: Let them accompany you\n2: Say it's okay""", false)
  private val multi0112 = Area("multi0112", s"""You're in the Civics class.|"It's nothing really, I can go by myself", your nervousness is showing.\n"Yeah… right…", eerily menacing they sound  "How about… YOU go back to your seat and stop FAKING your sickness. Oh, and… remember to visit the detention room this afternoon."\nYou couldn't keep up the act and they saw right through you.""", false)
  private var multi0111 = Area("multi0111", "", false)
  private val multi01111 = Area("multi01111", s"""You're at the infirmary.|"I... don't remember you mentioning a headache? And since when did your sniffling stop?"\nYou couldn't even keep your story straight. The nurse lets out an audible sigh.\n"Alright. This... theatrics ends now, you are wasting my time!"\n"I'm afraid I'll have to write you up. No, YOU should be afraid, young one."""", false)

  /** physical areas and ending areas */
  private val central = Area("Central", "You're at the central hallway.\nThe notice screen says: Our security called in sick today.\nContact the principle if you need to leave early.\n\nAsking them to help to sneak out isn't a good idea...", true)
  private val north = Area("North", "You're at the robotics room.\nThere's a group of student working on a project in the corner, best not disturb them.", true)
  private val south = Area("South", "You're at the South hallway.\nThe room 1.3 is empty today.", true)
  private val east = Area("East", "You're at the East hallway.\nYour best friend should be studying in room 1.2 now.\nYou can go down stairs from here.\nYou really shouldn't go up, all classes are full up there.", true)
  private var west = Area("West", s"""You're at the West hallway, right outside your class.|"Excuse me, can I go to the toilet?". This should be fine, people have sneaked out like this before.\n"Go", she doesn't even give you a look and continues on with her boring material.""", true)
  private val southwest = Area("Stairs", "You're at the stairs in the South West corner.\nYou really shouldn't go up, but nobody should notice you in this corner...", true)
  private var exit = Area("Exit", "The exit... it's locked with a padlock.\nYou've got to get out of here. Maybe you can pry the padlock open with some tools?", true)
  private var central0 = Area("Ground Central", "You're at the middle of the school grounds.\nThe notice screen says: Our security called in sick today.\nContact the principle if you need to leave early.\nYou can see the exit to the South East, across the yard.", true)
  private var north0 = Area("Ground North", "You're at the library in the north.\nA pyromania thought lights up, whatever that means...", true)
  private var south0 = Area("Ground South", "South of the school grounds, the Chemistry lab.\nIt's empty, maybe you can grab something real quick", true)
  private var east0 = Area("Ground East", s"""East of the school grounds, You're at the infirmary.\nToday's nurse really IS your favourite.|"You can go back to teaching now.", the nurse addresses your teacher.\n"Now, now. I know what's going on...", you can hear your heart pounding.\n"But, and this BETTER be a secret, here's a sick note for you, dear."""", true)
  private var west0 = Area("Ground West", "West of the school grounds, the Physics lab.\nThe lab instructor is inside. You can ask them for a tool.", true)
  private var southwest0 = Area("Ground Stairs ", "You're at the stairs in the South West corner.\nThe only way is up! Well, there's the principal's office over there... you're going to need a good reason before you ask them anything.", true)
  private val southwest2 = Area("2 Stairs", "Well, you can always go up...", true)
  private val southwest3 = Area("3 Stairs", "Might as well go to the rooftop, no?", true)
  private val southwest4 = Area("4 Stairs", "You really should've taken more of that cardio...", true)
  private val southwest5 = Area("5 Stairs", "Almost there... C'mon you", true)
  private val rooftop = Area("Rooftop", "Why\nare\nyou\nhere?\nAnd why is that railing to your North broken?", true)
  private val rooftopn = Area("Rooftop North", "This is a nice view of the city. It's really easy to jump down from here, someone should fix this!", true)
  private val jumped = Area("WHY", "There are other ways out.\nCall 0925250113 (Finnish, English, Swedish, Arabic) if you or some one you know are in a crisis and you need assistance.", false)
  private val street = Area("OUT", "Freedom.\nAt last", false)
  private val hospital = Area("Hospital", "You wake up in the hospital, cuffed to the sick bed.", false)
  private val destination = street

  /** a vector containing areas that you will lose (bad ending) if enter, most being multiple choice areas */
  var losing = Vector[Area]()

  /** setup of multiple choice phase, after the player chose a name */
  def multiSetup() =
    multi0 = Area("multi0", s"""You're in class.|"${pName}", they called out. You reluctantly raise your hand.\nIt's only 10:05, there's 85 minutes of boredom ahead.\n1: Sneak out\n2: It's probably fine""", false)
    multi02 = Area("multi02", s"""You're in class.|It's 10:30, you've been sleeping.\n"${pName}", the teacher yelled, "Enough is enough, detention!".\nThis is the third time they caught you sleeping this semester...""", false)
    multi0111 = Area("multi0111", s"""You're at the infirmary.|You're went to the infirmary with your teacher.\n"${pName}, is it? How are your feeling, dear", it's your favourite nurse. What a relief!\n1: "My head. It really hurts!"\n2: "It's really hard to breathe!", *sniffle*, "My throat sores a lot too."""", false)
    multi.setChoices(Vector(0 -> multi0))
    multi0.setChoices(Vector(1 -> multi01, 2 -> multi02))
    multi01.setChoices(Vector(1 -> multi011, 2 -> west))
    multi011.setChoices(Vector(1 -> multi0111, 2 -> multi0112))
    multi0111.setChoices(Vector(1 -> multi01111, 2 -> east0))
    losing = Vector(multi02, multi0112, multi01111, jumped)

  /** setup of the physical map, after the player gets through the multiple choice phase */
  def mapSetup(): Unit =
    central.setNeighbors(Vector("north" -> north, "east" -> east, "south" -> south, "west" -> west))
    central0.setNeighbors(Vector("north" -> north0, "east" -> east0, "south" -> south0, "west" -> west0, "southeast" -> exit))
    north.setNeighbors(Vector("south" -> central))
    north0.setNeighbors(Vector("south" -> central0))
    south.setNeighbors(Vector("north" -> central, "west" -> southwest))
    south0.setNeighbors(Vector("north" -> central0, "east" -> exit, "west" -> southwest0, "northeast" -> east0))
    east.setNeighbors(Vector("west" -> central, "down" -> east0))
    east0.setNeighbors(Vector("south" -> exit, "west" -> central0, "up" -> east, "southwest" -> south0))
    west0.setNeighbors(Vector("east" -> central0, "south" -> southwest0))
    west.setNeighbors(Vector("east" -> central, "south" -> southwest))
    southwest0.setNeighbors(Vector("north" -> west0, "east" -> south0, "up" -> southwest))
    southwest.setNeighbors(Vector("north" -> west, "east" -> south, "up" -> southwest2, "down" -> southwest0))
    southwest2.setNeighbors(Vector("up" -> southwest3, "down" -> southwest))
    southwest3.setNeighbors(Vector("up" -> southwest4, "down" -> southwest2))
    southwest4.setNeighbors(Vector("up" -> southwest5, "down" -> southwest3))
    southwest5.setNeighbors(Vector("up" -> rooftop, "down" -> southwest4))
    rooftop.setNeighbors(Vector("north" -> rooftopn, "down" -> southwest5))
    rooftopn.setNeighbors(Vector("south" -> rooftop, "down" -> jumped))
    exit.setNeighbors(Vector("north" -> east0, "west" -> south0, "northwest" -> central0))

  def itemSetup() =
    north.addItem(Item("motor", "A strong motor.\nYou've heard it's even strong enough to bend steel."))
    south0.addItem(Item("black bottle", "A black heavy bottle. The label says:\n\nH2SO4 96% (Ultrapur)\n      Danger\n    H290 - H314\n")) //A nasty bottle filled with acid
    southwest2.addItem(Item("red canister", "A large red canister.\nIt reeks of some kind of fuel.\nWhy did you find it there?")) //Fuel

  def NPCSetup() =
    east0.addNPC(NPC("nurse", s"""You ask them for a sick note.\n"It's Civics, isn't it..."\n"Here you go."\nYou get the sick note."""))
    west0.addNPC(NPC("instructor", s"""You ask them for a L-shaped wrench.\n"Remember to return it!"\nYou get the wrench"""))
    southwest0.addNPC(NPC("principal", s"""You ask them for a sick leave.\nThe principal checks the note for a while.\n"I'll go unlock the exit.""""))

  /** The maximum number of times you can go by your class before the teacher finds out. And the parameter's count. The default is as many as you like. */
  var teacherLimit = 99
  var teacherCount = 0

  /** The amount of turn you can take before you succumb to the smoke of the fire you create */
  var fireLimit = 99
  var fireCount = 0

  /** The character that the player controls in the game. */
  val player = Player(multi)
  player.name(pName)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 40
  /** returns true if the player is in a physical area */
  def started = this.player.location.physical
  /** returns if the exit is unlocked */
  var unlocked = false

  /** first good ending of multiple choice phase */
  def toilet() =
    east0 = Area("Ground East", "You're at the infirmary.\nToday's nurse is your favourite, you might be able to ask them for a favor", true)
    mapSetup()
    itemSetup()
    NPCSetup()

  /** 2nd good ending of multiple choice phase */
  def nurse() =
    west = Area("West", "You're at the West hallway, right outside your class.\nThe Civics teacher could notice you any moment!", true)
    this.player.addItem(Item("sick note", "a detailed sick note written for you by the lovely nurse"))
    teacherLimit = 3
    mapSetup()
    itemSetup()
    NPCSetup()
    east0.addNPC(NPC("nurse0", """"Give the note to the principal, dear.""""))

  /** carry out methods necessary when unlocking the exit, with some case specific methods */
  def unlock(): Unit =
    if this.player.unlockMethod == "burned" then
      north0 = Area("Ground North", "You're at the library.\nThere's a large fire in the back. Who started it???", true)
      this.player.goTo(north0)
      fireLimit = 2
    exit = Area("Exit1", "You're at the exit\nIt's not locked anymore, you're free to leave.", true)
    mapSetup()
    exit.setNeighbors(Vector("north" -> east0, "west" -> south0, "northwest" -> central0, "out" -> street))
    unlocked = true
    if this.player.location.name == "Exit" then this.player.goTo(exit)

  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.destination

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver =
    this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || losing.contains(this.player.location) || this.teacherLimit == this.teacherCount || this.fireLimit == this.fireCount

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = s"""(One of the endings based on true story)\nThis is a very short text-based game with multiple endings, controlled using text inputs. The main goal is to reach all endings.\nFeel free to use the provided map later on (in the menu), but first you have some choices to make.\nType "help" for further instructions."""

  /** when an ending is reached, update it as reached, returns description for goodbyeMessage */
  def goodbye1 =
    endings += ("Zzz...Zzz" -> true)
    "Zzz...Zzz... Game over!"
  def goodbye2 =
    endings += ("Humiliating!" -> true)
    "Humiliated in front of the class! Game over!"
  def goodbye3 =
    endings += ("Stick to your lies!" -> true)
    "Stick to your lies! Game over!"
  def goodbyeOK =
    endings += ("A-Are you okay?" -> true)
    "Help is always available at Aalto University"
  def goodending = this.player.unlockMethod match
    case "principal" => "Thank god for the nurse."
    case "pried" => "Lucky it ran on battery!"
    case "dissolved" => "Dexterity 100!"
    case "burned" => "Certified pyromaniac"
    case other => ""

  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on the player's actions */
  def goodbyeMessage =
    val a =
      if this.isComplete then
        endings += (goodending -> true)
        s"${goodending}"
      else if this.turnCount == this.timeLimit then
        "Oh no! The principal stands behind you. They noticed you wandering around during class!\nGame over! You should've been quicker!\nYou will not unlock any new ending."
      else if this.teacherLimit == this.teacherCount then
        endings += ("You already lied once..." -> true)
        "The Civics teacher noticed. They barge out towards you.\nYou're not gonna be able to lie to them again. Game over!"
      else if this.fireCount == this.fireLimit then
        endings += ("Fool around and find out!" -> true)
        this.player.goTo(hospital)
        "You pass out due to inhaling too much smoke. Luckily you were rescued by the nurse.\nThe police got your fingerprints from a fuel can they found...\nFool around and find out!"
      else if this.player.hasQuit then ""
      else
        val x = this.player.location.name
        x match
          case "multi02" => goodbye1
          case "multi0112" => goodbye2
          case "multi01111" => goodbye3
          case "WHY" => goodbyeOK
          case oher => ""
    a + s"""\n\n𝐘𝐨𝐮 𝐜𝐚𝐧 𝐭𝐫𝐲 𝐚𝐠𝐚𝐢𝐧 𝐟𝐨𝐫 𝐦𝐨𝐫𝐞 𝐞𝐧𝐝𝐢𝐧𝐠𝐬. The option is available in the "Program" menu."""

  /** goodbye message after all endings achieved */
  def finishedGoodbye = goodbyeMessage.dropRight(65)+"."

  /** list endings */
  def ends(): String =
    val a = endings.filter(x => x._2).keys
    if a.isEmpty then "No endings reached."
    else "Endings reached: " + a.mkString("\"", "\", \"", "\"")

  /** methods used in playTurn when in multiple-choice phase,
    * or in niche cases that would be easier to implement in this class instead of the Player (for me)*/
  def multiTurn(command: String): String = //multiple-choice
    if turnCount == 0 then
      if pName == "" then
        pName = command.toLowerCase.split(' ').map(_.capitalize).mkString(" ")
        this.player.name(pName)
      multiSetup()
      this.player.choose(0)
      this.turnCount += 1
      this.player.location.description.dropWhile(_ != '|').drop(1)
    else if command == "1" || command == "2" then
      this.player.choose(command.toInt)
      if this.player.location.name == "Ground East" then nurse()
      if this.player.location.name == "West" then toilet()
      this.turnCount += 1
      this.player.location.description.dropWhile(_ != '|').drop(1)
    else if command == "endings" then this.player.location.description.dropWhile(_ != '|').drop(1) + "\n\n" + ends()
    else if command == "quit" then this.player.quit()
    else s"""𝗪𝗿𝗶𝘁𝗲 𝗼𝗻𝗹𝘆 "𝟭" 𝗼𝗿 "𝟮"\n""" + this.player.location.description.dropWhile(_ != '|').drop(1)

  def useTurn(itemName: String): String = //use item
    if this.player.has(itemName) then
      val x = this.player.location.name
      if x == "Exit" || x == "Ground North" then
        if x == "Exit" then
          if (itemName == "motor" || itemName == "wrench") then
            if this.player.canPry then
              if this.player.unlocked then "You can't use that. It's already unlocked."
              else
                this.player.pry()
                unlock()
                "You create a leverage system by using the wrench with the motor and easily pry the lock open."
            else "You need something else to use that with!"
          else if itemName == "black bottle" then
            if !this.player.unlocked then
              this.player.dissolve()
              unlock()
              "You slowly pour the content of the bottle onto the padlock's shackle as it dissolves the.\nIt took some time, but the lock drops to the ground, shaft-less.\nHow did none of the acid get on you???"
            else "You can't use that here."
          else "You can't use that here."
        else if (itemName == "red canister" || itemName == "black bottle") then
          if this.player.canBurn then
            if this.player.unlocked then
              if this.player.unlockMethod == "burned" then "The alarms are going off... Somebody already lit this place on fire."
              else "The lock is already down. You're not gonna bring the school down too right?\nRight?"
            else
              this.player.burn()
              unlock()
              "You empty the fuel in the canister onto the floor.\nYou grab a book and pour some acid on to heat it up then throw it on the ground.\nA large fire breaks out, the alarm goes off. Luckily there wasn't anyone in there...\nThe exit should be unlocked now, not that you need to go back to class..."
          else "You need something else to use that with!"
        else "You can't use that here."
      else "Use it on... what?"
    else "You don't have that."

  def pryTurn() = //pry the lock open
    if this.player.location.name == "Exit" then
      if this.player.canPry then
        if this.player.unlocked then "Pry what? It's already unlocked."
        else
          this.player.pry()
          unlock()
          "You create a leverage system by using the wrench with the motor and easily pry the lock open."
      else "You don't have the tools to do that."
    else "Pry what?"

  def burnTurn() = //burn the library down
    if this.player.location.name == "Ground North" then
      if this.player.canBurn then
        if !this.player.unlocked then
          this.player.burn()
          unlock()
          "You empty the fuel in the canister onto the floor.\nYou grab a book and pour some acid on to heat it up then throw it on the ground.\nA large fire breaks out, the alarm goes off. Luckily there wasn't anyone in there...\nThe exit should be unlocked now, not that you need to go back to class..."
        else if this.player.unlockMethod == "burned" then "The alarms are going off... Somebody already lit this place on fire."
        else "The lock is already down. You're not gonna bring the school down too right?\nRight?"
      else "You don't have enough materials to start a fire."
    else "Burn what?"


  /** Plays a turn by executing the given in-game command, such as “go west”. Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String): String =

    if this.player.location.name.contains("multi") && command != "help" then //multiple-choice phase
      multiTurn(command)
    else
      if command == "endings" then ends() //print list of achieved endings
      else
        val action = Action(command)
        val outcomeReport = action.execute(this.player)
        if outcomeReport.isDefined && command != "help" then
          this.turnCount += 1
        if this.player.location.name == "West" then
          this.teacherCount += 1
        if this.player.location.name == "Ground North" && this.player.unlockMethod == "burned" then
          this.fireCount += 1
        if this.player.unlocked && !unlocked then
          this.unlock()
        outcomeReport.getOrElse("") match
          case x if x.contains("use|") => useTurn(x.drop(4))
          case "" => s"Unknown command: \"$command\"."
          case x if x == "pry." => pryTurn()
          case x if x == "burn." => burnTurn()
          case other => other

end Adventure

