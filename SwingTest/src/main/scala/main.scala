import swing._
import swing.event._


// Class SimpleSwingApplication replaces SimpleGUIApplication in Scala 2.8, 
// so second case is deprecated.
object SwingApp extends SimpleSwingApplication {

    // Top method must be implemented in object that
    // extends SimpleSwingApplication.
    // Top returns Frame (standard OS window with generic elements).
    // In this case top returns MainFrame. That means,
    // when we close the window, the application starting to shut down.
    def top = new MainFrame {

        title = "SwingApp"
        var numclicks = 0

        object label extends Label {

            val prefix = "Number of button clicks: "
            text = prefix + "0  " 
            listenTo(button)

            reactions += {

                case ButtonClicked(button) =>
                    numclicks = numclicks + 1
                    text = prefix + numclicks
            }
        }

        object button extends Button {
            text = "I am a button"
        }

        contents = new FlowPanel {

            contents.append(button, label)
            border = Swing.EmptyBorder(5, 5, 5, 5)
        }
    }
}
