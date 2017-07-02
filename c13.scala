/*
The hex encoded string:
1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
has been XOR'd against a single character. Find the key, decrypt the message.
You can do this by hand. But don't: write code to do it for you.
How? Devise some method for "scoring" a piece of English plaintext. Character
 frequency is a good metric. Evaluate each output and choose the one with the best score.

 Step 1: Convert hex string to decimal
 Step 1.1: Break up string into Array of hex characters
 Step 1.2: Convert each hex character in decimal and xor it with a potential key
 Step 1.3: Convert each character to its ascii character equivalent
 Step 2: Score the plain text array (letter frequency)
 Step 3: compare the scores of each plain text and choose the best scoring one
*/
package c13

import c12.C12

class C13 {
  def scorePlainText(plainText: String): Int = {
    8
  }

  def convertDecimalArrayToPlainText(decimalArray: Array[Int]): String = {
    decimalArray
      .map(_.toChar)
      .mkString("")
  }

  def findBestKey(decimalArray: Array[Int]): Char = {
    var bestScore = 0

    for (i <- 32 to 126) {
      // XOR with the decimal i
      val xored = decimalArray.map(_ ^ i)
      val plainText = convertDecimalArrayToPlainText(xored)
      val currScore = scorePlainText(plainText)
    }

    'o'
  }

  def getTheBestKey(hexStr: String): Char = {
    // Get the an array containing the decimal conversions of each hex digit
    val c = new C12
    val decimalArray = c.convertHexArrayToDecimalArray(hexStr.split("(?<=\\G..)"))

    // Try xoring with each possible character and score the plaint text
    val bestKey = findBestKey(decimalArray)
    bestKey
  }
}
