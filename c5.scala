package c5

class C5 {
  def applyRepeatingKeyToText(i: Int, key: Array[Char], text: Array[Char], encodedText: Array[Int]): Array[Int] = {
    if (i >= text.length) {
      encodedText
    }
    else {
      val subText = text.slice(i, i + (key.length))
      val encodedSubArray = subText.zip(key).map{ case (x, y) => x ^ y }

      applyRepeatingKeyToText(i+key.length, key, text, encodedText ++ encodedSubArray)
    }
  }

  def convertDecimalArrayToHexString(i: Int, text: Array[Int], hexEncodedText: String): String = {
    if (i == text.length){
      hexEncodedText
    }
    else {
      val hexedNumber = "%x".format(text(i))
      convertDecimalArrayToHexString(i+1, text, hexEncodedText + hexedNumber)
    }
  }

  def encodeStringWithRepeatingKeyXOR(plaintext: String, key: String): String = {
    val textArray = plaintext.toArray
    val keyArray = key.toArray

    val xoredArray = applyRepeatingKeyToText(0, keyArray, textArray, Array[Int]())

    convertDecimalArrayToHexString(0, xoredArray, "")
  }
}
