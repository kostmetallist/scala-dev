package com.github.kostmetallist

import  swing._
import  swing.event._
import  javax.swing.ImageIcon


// Class SimpleSwingApplication replaces SimpleGUIApplication in Scala 2.8, 
// so second case is deprecated.

object Grintu extends SimpleSwingApplication {

    // Top method must be implemented in object that
    // extends SimpleSwingApplication.
    // Top returns Frame (standard OS window with generic elements).
    // In this case top returns MainFrame. That means,
    // when we close the window, the application starting to shut down.
    def top = new MainFrame {

        title = "GrIntu v0.0.1"

        object chartLabel extends Label {

            icon = new ImageIcon("./data/chart.png")
        }

        contents = new BorderPanel {

            //layout(label) = BorderPanel.Position.West
            //layout(button) = BorderPanel.Position.East

            layout(chartLabel) = BorderPanel.Position.West
            layout += new GridPanel(5, 1) {

                contents += new Label("Function: ")
                contents += new TextField
                contents += new Label("Precision: ")
                contents += new Slider
                contents += new Button("Generate")

            } -> BorderPanel.Position.East
        }
    }
}

