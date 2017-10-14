package c8

import scala.io.Source
import util.control.Breaks._

object C8 {
  def detectECB(encryptedText: Array[String]): Unit = {
    for (i <- 0 until encryptedText.length) {
      val text = encryptedText(i)
      val bytes = for (j <- List.range(0, text.length, 32)) yield text.substring(j, j+32)
      if (bytes.length != bytes.toSet.size){
        break
      }
    }
  }

  def solution(): Unit = {
    val encryptedText = Source.fromResource("8.txt").getLines.toArray
    println(detectECB(encryptedText))
  }
}