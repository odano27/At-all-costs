package adventure.ui

import scala.swing.*
import scala.swing.event.*
import javax.swing.UIManager
import adventure.Adventure
import smcl.pictures.Picture
import java.io.File //used to open the map file from within the .jar

import java.awt.{Dimension, Insets, Point}
import scala.language.adhocExtensions // enable extension of Swing classes

object AdventureGUI extends SimpleSwingApplication :
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  def top = new MainFrame :

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
    def won = endings.forall(_._2)
    // Access to the application’s internal logic:
    var name = ""
    var game = Adventure(name)
    var player = game.player

    // Components:

    val locationInfo = new TextArea(7, 80) :
      editable = false
      wordWrap = true
      lineWrap = true
    val turnOutput = new TextArea(7, 80) :
      editable = false
      wordWrap = true
      lineWrap = true
    val input = new TextField(40) :
      minimumSize = preferredSize
    this.listenTo(input.keys)
    val turnCounter = Label()

    // Events:

    this.reactions += {
      case keyEvent: KeyPressed =>
        if keyEvent.source == this.input && keyEvent.key == Key.Enter && !this.game.isOver then
          val command = this.input.text.trim
          if command.nonEmpty then
            this.input.text = ""
            this.playTurn(command)
    }

    // Layout:

    this.contents = new GridBagPanel :

      import scala.swing.GridBagPanel.Anchor.*
      import scala.swing.GridBagPanel.Fill

      layout += Label("Location:") -> Constraints(0, 0, 1, 1, 0, 1, NorthWest.id, Fill.None.id, Insets(8, 5, 5, 5), 0, 0)
      layout += Label("Command:") -> Constraints(0, 1, 1, 1, 0, 0, NorthWest.id, Fill.None.id, Insets(8, 5, 5, 5), 0, 0)
      layout += Label("Events:") -> Constraints(0, 2, 1, 1, 0, 0, NorthWest.id, Fill.None.id, Insets(8, 5, 5, 5), 0, 0)
      layout += turnCounter -> Constraints(0, 3, 2, 1, 0, 0, NorthWest.id, Fill.None.id, Insets(8, 5, 5, 5), 0, 0)
      layout += locationInfo -> Constraints(1, 0, 1, 1, 1, 1, NorthWest.id, Fill.Both.id, Insets(5, 5, 5, 5), 0, 0)
      layout += input -> Constraints(1, 1, 1, 1, 1, 0, NorthWest.id, Fill.None.id, Insets(5, 5, 5, 5), 0, 0)
      layout += turnOutput -> Constraints(1, 2, 1, 1, 1, 1, SouthWest.id, Fill.Both.id, Insets(5, 5, 5, 5), 0, 0)

    // create a new adventure and player instance, update the GUI
    def tryAgain =
      this.endings = game.endings
      game = Adventure(name)
      game.endingsReached(this.endings)
      this.player = game.player
      updateInfo(this.game.welcomeMessage)
      this.input.enabled = true
      this.pack()
      this.input.requestFocusInWindow()

    //opens the map image file
    def Img():Unit =
      var a = new File(AdventureGUI.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).getPath
      a=a.dropRight(a.length-a.lastIndexOf('\\')-1) + "map.png"
      Runtime.getRuntime.exec(s"""cmd /c "${a}"""")



    // Menu:
    this.menuBar = new MenuBar :
      contents += new Menu("Program") :
        val quitAction = Action("Quit")(dispose())
        val retry = Action("Try again")(tryAgain)
        val map = Action("Map")(Img())
        contents += MenuItem(quitAction)
        contents += MenuItem(retry)
        contents += MenuItem(map)


    // Set up the GUI’s initial state:
    this.title = game.title
    this.updateInfo(this.game.welcomeMessage)
    this.location = Point(600, 200)
    this.minimumSize = Dimension(700, 500)
    this.pack()
    this.input.requestFocusInWindow()


    def playTurn(command: String) =
      val turnReport = this.game.playTurn(command)
      this.updateInfo(turnReport)
      this.input.enabled = !this.game.isOver


    def updateInfo(info: String) =
      name = game.pName
      this.title = game.title
      if !this.game.isOver then
        this.turnOutput.text = info
      else
        if !won then
          this.turnOutput.text = info + "\n" + this.game.goodbyeMessage
        else
          this.turnOutput.text = info + "\n" + this.game.finishedGoodbye //display a slightly different text if all endings have been reached
        this.endings = game.endings //update endings list when an ending is reached
      if !won then
        this.locationInfo.text = this.player.location.fullDescription
        val instructions =
          if this.game.started && !this.game.isOver then """    Type "help" for instructions. 𝐋𝐨𝐨𝐤 𝐨𝐮𝐭 𝐟𝐨𝐫 𝐚𝐜𝐭𝐢𝐨𝐧 𝐜𝐮𝐞𝐬 (more info in the instructions).""" else "    Type \"help\" for instructions. "
        this.turnCounter.text = s"Endings reached: ${endings.count(x => x._2)}/${endings.size} | Turns played: " + this.game.turnCount + instructions
      else //display a congratulation message when all endings have been reached
        this.turnCounter.text = s"Endings reached: ${endings.count(x => x._2)}/${endings.size} | Turns played: " + this.game.turnCount
        this.locationInfo.text = this.player.location.fullDescription + "\n\nCongratulations!!! You've reached all endings."

  end top

  // Enable this code to work even under the -language:strictEquality compiler option:
  private given CanEqual[Component, Component] = CanEqual.derived

  private given CanEqual[Key.Value, Key.Value] = CanEqual.derived

end AdventureGUI

