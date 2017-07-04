package c4

import scala.io.Source
import c3.C3

class C4 {
  def getLinesFromFile(): List[String] = {
    Source
      .fromFile("4.txt")
      .getLines
      .toList
  }

  def getBestKeyFromLines(lines: List[String], c: C3, bestLine: String, bestScore: Double): String = {
    // Use breakSIngleByeXORCipher on each line.
    // Use this to get the lowest plain text score
    lines match {
      case h :: t => {
        val decimalArray = h.split("(?<=\\G..)").map(Integer.parseInt(_, 16))
        val (lineKey, lineScore) = c.findBestKey(decimalArray, 0, Double.MaxValue, 0)
        //println(h.split("(?<=\\G..)").map(Integer.parseInt(_, 16)).map(_ ^ lineKey).map(_.toChar).mkString(""))

        if (lineScore < bestScore){
          val decoded = h.split("(?<=\\G..)").map(Integer.parseInt(_, 16)).map(_ ^ lineKey).map(_.toChar).mkString("")
          getBestKeyFromLines(t, c, decoded, lineScore)
        }
        else {
          getBestKeyFromLines(t, c, bestLine, bestScore)
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
    val line = getBestKeyFromLines(fileLines, c = new C3, "", Double.MaxValue)
    println(s"Line: $line")
    //line.split("(?<=\\G..)").map(_ ^ i).map(_.toChar).mkString("")
    line
    //"present day, present time"
  }
}
