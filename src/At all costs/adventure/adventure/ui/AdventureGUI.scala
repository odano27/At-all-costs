package adventure.ui

import scala.swing.*
import scala.swing.event.*
import javax.swing.UIManager
import adventure.{Adventure, Ending}
import java.awt.Desktop
import java.io.{File, IOException}

import java.awt.{Dimension, Insets, Point}
import scala.language.adhocExtensions // enable extension of Swing classes

object AdventureGUI extends SimpleSwingApplication :
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  def top = new MainFrame :

    // Access to the application’s internal logic. The player's name and the endings they
    // have reached are carried over from one play-through to the next.
    var name = ""
    var endings = Set[Ending]()
    var game = Adventure(name)
    def won = endings.size == Ending.values.length

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
    def tryAgain() =
      game = Adventure(name, endings)
      updateInfo(this.game.welcomeMessage)
      this.input.enabled = true
      this.pack()
      this.input.requestFocusInWindow()

    /** Opens the map image (map.png) that sits in the same directory as the game, using the
      * system's default image viewer. */
    def openMap(): Unit =
      val codeLocation = File(AdventureGUI.getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
      val candidates = Vector(File(codeLocation.getParentFile, "map.png"), File("map.png"))
      candidates.find(_.isFile) match
        case None =>
          Dialog.showMessage(this.contents.head, "Could not find map.png. Keep it in the same folder as the game.", "Map", Dialog.Message.Error)
        case Some(mapFile) =>
          try Desktop.getDesktop.open(mapFile)
          catch case _: (IOException | UnsupportedOperationException) =>
            Dialog.showMessage(this.contents.head, s"Could not open the map. You can find it at:\n${mapFile.getAbsolutePath}", "Map", Dialog.Message.Error)

    // Menu:
    this.menuBar = new MenuBar :
      contents += new Menu("Program") :
        val quitAction = Action("Quit")(dispose())
        val retry = Action("Try again")(tryAgain())
        val map = Action("Map")(openMap())
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
      name = game.playerName
      endings = game.reachedEndings
      this.title = game.title
      this.turnOutput.text = if this.game.isOver then info + "\n" + this.game.goodbyeMessage else info
      val instructions =
        if won then ""
        else if this.game.started && !this.game.isOver then """    Type "help" for instructions. 𝐋𝐨𝐨𝐤 𝐨𝐮𝐭 𝐟𝐨𝐫 𝐚𝐜𝐭𝐢𝐨𝐧 𝐜𝐮𝐞𝐬 (more info in the instructions)."""
        else "    Type \"help\" for instructions. "
      this.turnCounter.text = s"Endings reached: ${endings.size}/${Ending.values.length} | Turns played: ${this.game.turnCount}" + instructions
      val congratulations = if won then "\n\nCongratulations!!! You've reached all endings." else ""
      this.locationInfo.text = this.game.player.location.fullDescription + congratulations

  end top

  // Enable this code to work even under the -language:strictEquality compiler option:
  private given CanEqual[Component, Component] = CanEqual.derived

  private given CanEqual[Key.Value, Key.Value] = CanEqual.derived

end AdventureGUI

