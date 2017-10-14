package c8

import c7.C7

object C8 {
  def solution(): String = {
    val encryptedText = Source.fromResource("8.txt").getLines.mkString("")
    val key = "YELLOW SUBMARINE"
    decrypt(encryptedText, key)
    C7.    
  }
}