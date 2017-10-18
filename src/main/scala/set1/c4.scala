package set1.c4

import scala.io.Source
import set1.c1.C1
import set1.c2.C2
import set1.c3.C3

object C4 {
  // Returns an array containing each line from 4.txt as separate elements
  def getLinesFromFile(fileName: String): List[String] = {
    Source
      .fromResource(fileName)
      .getLines
      .toList
  }

  def getSingleCharXORLineFromLines(lines: List[String], bestLine: String, bestScore: Double, bestKey: Int): String = {
    // Use breakSIngleByeXORCipher on each line.
    // Use this to get the lowest plain text score
    lines match {
      case h :: t => {
        val hArray = C1.splitStringIntoArray(h, 2)
        val decimalArray = C2.convertHexArrayToDecimalArray(hArray)
        val (lineKey, lineScore) = C3.findBestKey(decimalArray, 0, Double.MinValue, 0)

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
    val lines = getLinesFromFile("4.txt")

    val xoredLine = getSingleCharXORLineFromLines(lines, "", Double.MinValue, 0)
    
    xoredLine
  }

  def solution(): String = {
    detectSingleCharacterXOR()
  }
}
