package c14

import scala.io.Source
import c13.C13

class C14 {
  def getLinesFromFile(filename: String): Array[String] = {
    Source
      .fromFile(filename)
      .getLines
      .toArray
  }

  def getBestKeyFromLines(lines: Array[String], c: C13, bestLine, bestScore): Unit = {
    // Use breakSIngleByeXORCipher on each line.
    // Use this to get the lowest plain text score
    lines match {
      case (h,t) => {
        val lineScore = c.findBestKey(h, 0, Double.MaxValue, 0)
        if (lineScore < bestScore){
          getBestKeyFromLines(t, c, h, lineScore)
        }
        else {
          getBestKeyFromLines(t, c, bestLine, bestScore)
        }
      }
      case (_) => {
        // return the keys here?
        bestLine
      }
    }
  }

  def detectSingleCharacterXOR(filename: String): String = {
    // Open the file and create an array of the lines
    val fileLines = getLinesFromFile(filename)
    "present day, present time"
  }
}
