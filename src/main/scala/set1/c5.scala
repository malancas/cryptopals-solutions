package set1.c5

import scala.annotation.tailrec

object C5 {
  // If the digit is less than 16, 0 must be added
  // to the beginning of the hex representation
  def convertDecimalToHex(digit: Int): String = {
    if (digit < 16){
      "0" + digit.toHexString
    }
    else {
      digit.toHexString
    }
  }
  
  @tailrec
  final def applyRepeatingKeyToText(i: Int, key: Array[Char], text: Array[Char], encodedText: Array[Int]): Array[Int] = {
    if (i >= text.length) {
      encodedText
    }
    else {
      // Determine how long the next chunk of text to be encoded is. This will change at the end of
      // the string, when the length of the key might be larger than the remaining text left to process
      val subTextLength = if (i + key.length >= text.length) (key.length - (i+key.length - text.length)) else key.length
      val subText = text.slice(i, i+subTextLength)
      val encodedSubArray = subText.zip(key.slice(0, subTextLength+1)).map{ case (x, y) => x ^ y }

      applyRepeatingKeyToText(i+key.length, key, text, encodedText ++ encodedSubArray)
    }
  }

  @tailrec
  final def convertDecimalArrayToHexString(i: Int, text: Array[Int], hexEncodedText: String): String = {
    if (i == text.length){
      hexEncodedText
    }
    else {
      val hexedNumber = convertDecimalToHex(text(i))
      convertDecimalArrayToHexString(i+1, text, hexEncodedText + hexedNumber)
    }
  }

  def encodeStringWithRepeatingKeyXOR(plaintext: String, key: String): String = {
    val textArray = plaintext.toArray
    val keyArray = key.toArray

    val xoredArray = applyRepeatingKeyToText(0, keyArray, textArray, Array[Int]())
    val xoredStr = xoredArray.mkString(" ")

    convertDecimalArrayToHexString(0, xoredArray, "")
  }

  def solution(plaintext: String, key: String): String = {
    encodeStringWithRepeatingKeyXOR(plaintext, key)
  }
}
