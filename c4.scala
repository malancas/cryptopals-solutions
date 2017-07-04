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

  def getBestKeyFromLines(lines: List[String], bestLine: String, bestScore: Double): String = {
    // Use breakSIngleByeXORCipher on each line.
    // Use this to get the lowest plain text score
    lines match {
      case h :: t => {
        val hArray = c1.splitStringIntoArray(h, 2)
        val decimalArray = c2.convertHexArrayToDecimalArray(hArray)
        val (lineKey, lineScore) = c3.findBestKey(decimalArray, 0, Double.MaxValue, 0)
        //println(h.split("(?<=\\G..)").map(Integer.parseInt(_, 16)).map(_ ^ lineKey).map(_.toChar).mkString(""))

        if (lineScore < bestScore){
          val decoded = decimalArray
            .map(_ ^ lineKey)
            .map(_.toChar)
            .mkString("")

          getBestKeyFromLines(t, decoded, lineScore)
        }
        else {
          getBestKeyFromLines(t, bestLine, bestScore)
        }
      }
      case _ => {
        // return the keys here?
        println(bestScore)
        bestLine
      }
    }
  }

  def detectSingleCharacterXOR(): String = {
    // Open the file and create an array of the lines
    val fileLines = getLinesFromFile()
    val line = getBestKeyFromLines(fileLines, "", Double.MaxValue)
    println(s"Line: $line")
    //line.split("(?<=\\G..)").map(_ ^ i).map(_.toChar).mkString("")
    line
    //"present day, present time"
  }
}
