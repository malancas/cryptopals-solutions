package c4

import scala.io.Source
import c1.C1
import c2.C2
import c3.C3

class C4 {
  val c1 = new C1
  val c2 = new C2
  val c3 = new C3

  // Returns an array containing each line from 4.txt as separate elements
  def getLinesFromFile(): List[String] = {
    Source
      .fromFile("4.txt")
      .getLines
      .toList
  }

  def getSingleCharXORLineFromLines(lines: List[String], bestLine: String, bestScore: Double, bestKey: Int): String = {
    // Use breakSIngleByeXORCipher on each line.
    // Use this to get the lowest plain text score
    lines match {
      case h :: t => {
        val hArray = c1.splitStringIntoArray(h, 2)
        val decimalArray = c2.convertHexArrayToDecimalArray(hArray)
        val (lineKey, lineScore) = c3.findBestKey(decimalArray, 0, Double.MinValue, 0)

        if (lineScore > bestScore){
          val decoded = decimalArray
            .map(_ ^ lineKey)
            .map(_.toChar)
            .mkString("")

          getSingleCharXORLineFromLines(t, decoded, lineScore, lineKey)
        }
        else {
          getSingleCharXORLineFromLines(t, bestLine, bestScore, bestKey)
        }
      }
      case _ => {
        bestLine
      }
    }
  }

  def detectSingleCharacterXOR(): String = {
    // Open the file and get a list of the lines
    val lines = getLinesFromFile()

    val xoredLine = getSingleCharXORLineFromLines(lines, "", Double.MinValue, 0)
    
    xoredLine
  }
}
