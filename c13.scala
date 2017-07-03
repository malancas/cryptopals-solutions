/*
The hex encoded string:
1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
has been XOR'd against a single character. Find the key, decrypt the message.
You can do this by hand. But don't: write code to do it for you.
How? Devise some method for "scoring" a piece of English plaintext. Character
 frequency is a good metric. Evaluate each output and choose the one with the best score.
*/
package c13

import c12.C12

class C13 {
  // Sourced from: https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
  val referenceHash = Map('e' -> 12.02, 't' -> 9.10, 'a' -> 8.12, 'o' -> 7.68, 'i' -> 7.31, 'n' -> 6.95,
    's' -> 6.28, 'r' -> 6.02, 'h' -> 5.92, 'd' -> 4.32, 'l' -> 3.89, 'u' -> 2.88, 'c' -> 2.71, 'm' -> 2.61,
    'f' -> 2.30, 'y' -> 2.11, 'w' -> 2.09, 'g' -> 2.03, 'p' -> 1.82, 'b' -> 1.49, 'v' -> 1.11, 'k' -> 0.69,
    'x' -> 0.17, 'q' -> 0.11, 'j' -> 0.10, 'z' -> 0.07)

  def scorePlainText(plainText: String): Double = {
    // Make all letters lowercase for easier letter frequency analysis
    val loweredCase = plainText.toLowerCase()

    val frequencyHash = plainText.groupBy(c => c).mapValues(_.length.toDouble / plainText.length)

    var diffScore = 0.0
    for (i <- 0 to 256) {

      diffScore += math.abs(frequencyHash.getOrElse(i.toChar, 0.0) - referenceHash.getOrElse(i.toChar, 0.0))
    }
    diffScore
  }

  def convertDecimalArrayToPlainTextArray(decimalArray: Array[Int]): Array[Char] = {
    decimalArray
      .map(_.toChar)
  }

  def findBestKey_recurse(decimalArray: Array[Int], i: Int, bestScore: Double, bestKey: Int): Int = {
    if (i == 256){
      bestKey
    }
    else {
      // XOR with the decimal i
      val xored = decimalArray.map(_ ^ i).map(_.toChar).mkString("")
      val plaintext = xored.map(_.toChar).mkString("")
      val currScore = scorePlainText(plaintext)

      if (currScore < bestScore){
        findBestKey_recurse(decimalArray, i+1, currScore, i)
      }
      else {
        findBestKey_recurse(decimalArray, i+1, bestScore, bestKey)
      }
    }
  }

  def findBestKey(decimalArray: Array[Int]): Int = {
    var bestScore = Double.MaxValue
    var bestKey = 0
    var bestPlainText = ""

    for (i <- 0 to 256) {
      // XOR with the decimal i
      val xored = decimalArray.map(_ ^ i).map(_.toChar).mkString("")
      val plaintext = xored.map(_.toChar).mkString("")
      val currScore = scorePlainText(plaintext)

      if (currScore < bestScore){
        bestScore = currScore
        bestKey = i
        bestPlainText = plaintext
      }
     
    }

    println(bestPlainText)
    bestKey
  }

  def breakSingleByteXORCipher(hexStr: String): Int = {
    // Get the an array containing the decimal conversions of each hex digit
    val c = new C12
    val decimalArray = c.convertHexArrayToDecimalArray(hexStr.split("(?<=\\G..)"))

    // Try xoring with each possible character and score the plaint text
    val bestKey = findBestKey_recurse(decimalArray, 0, Double.MaxValue, 0)
    bestKey
  }
}
