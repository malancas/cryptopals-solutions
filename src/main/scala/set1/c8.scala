package set1.c8

import scala.io.Source

object C8 {
  def areThereRepetitions(text: String): String = {
    val bytes = for (j <- List.range(0, text.length, 32)) yield text.substring(j, j+32)
    if (bytes.length != bytes.toSet.size) text
    else ""
  }

  def detectECB(encryptedText: Array[String]): Array[String] = {
    val ecbEncoded = encryptedText.map(areThereRepetitions(_)).filterNot(word => word.isEmpty)
    ecbEncoded
  }

  def solution(): Unit = {
    val encryptedText = Source.fromResource("8.txt").getLines.toArray
    assert(detectECB(encryptedText).length > 0)
  }
}