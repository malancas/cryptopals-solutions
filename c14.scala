package c14

import scala.io.Source
import c13.C13

C14 {
  def getLinesFromFile(filename: String): Array[String] = {
    Source
      .fromFile(filename)
      .getLines
      .toArray
  }

  def getBestKeyFromLines(lines: Array[String]) = {
    c = new C13
    // Use breakSIngleByeXORCipher on each line.
    // Use this to get the lowest plain text score
  }

  def detectSingleCharacterXOR(filename: String): String = {
    // Open the file and create an array of the lines
    val fileLines = getLinesFromFile(filename)
  }
}
