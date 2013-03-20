package org.rorick.stairbook.scells

import swing._

/**
 * Main class of SCells spreadsheet application.
 */
object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "ScalaSheet"
    contents = new Spreadsheet(100, 26)
  }
}
