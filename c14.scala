/*

Detect single-character XOR

One of the 60-character strings in this file has been encrypted by single-character XOR.

Find it.

(Your code from #3 should help.)

 */

package c14

import scala.io.Source

C14 {
  def getLinesFromFile(filename: String): Array[String] = {
    Source
      .fromFile(filename)
      .getLines
      .toArray
  }

  def detectSingleCharacterXOR(filename: String): String = {
    // Open the file and create an array of the lines
    val fileLines = getLinesFromFile(filename)
  }
}
