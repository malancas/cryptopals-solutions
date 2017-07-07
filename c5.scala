package c5

class C5 {
  def convertDecimalToHex(digit: Int): String = {
    if (digit < 16){
      "0" + digit.toHexString
    }
    else {
      digit.toHexString
    }
  }

  def applyRepeatingKeyToText(i: Int, key: Array[Char], text: Array[Char], encodedText: Array[Int]): Array[Int] = {
    if (i >= text.length) {
      encodedText
    }
    else {
      val chunk = if (i + key.length >= text.length) (key.length - (i+key.length - text.length)) else key.length
      //println(chunk)
      val subText = text.slice(i, i+chunk)
      val encodedSubArray = subText.zip(key.slice(0, chunk+1)).map{ case (x, y) => x ^ y }

      applyRepeatingKeyToText(i+key.length, key, text, encodedText ++ encodedSubArray)
    }
  }

  def convertDecimalArrayToHexString(i: Int, text: Array[Int], hexEncodedText: String): String = {
    if (i == text.length){
      hexEncodedText
    }
    else {
      //val hexedNumber = "%x".format(text(i))
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
}
