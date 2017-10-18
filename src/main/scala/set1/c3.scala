package set1.c3

import scala.annotation.tailrec
import set1.c1.C1
import set1.c2.C2

object C3 {
  val referenceMap = Map('a' -> 0.0651738, 'b' -> 0.0124248, 'c' -> 0.0217339, 'd' -> 0.0349835,
    'e' -> 0.1041442, 'f' -> 0.0197881, 'g' -> 0.0158610, 'h' -> 0.0492888, 'i' -> 0.0558094,
    'j' -> 0.0009033, 'k' -> 0.0050529, 'l' -> 0.0331490, 'm' -> 0.0202124, 'n' -> 0.0564513,
    'o' -> 0.0596302, 'p' -> 0.0137645, 'q' -> 0.0008606, 'r' -> 0.0497563, 's' -> 0.0515760,
    't' -> 0.0729357, 'u' -> 0.0225134, 'v' -> 0.0082903, 'w' -> 0.0171272, 'x' -> 0.0013692,
    'y' -> 0.0145984, 'z' -> 0.0007836, ' ' -> 0.1918182)

  @tailrec
  final def makePlaintextScore(i: Int, score: Double, plaintext: String): Double = {
    if (i == plaintext.length) { score }
    else {
      val letterScore = referenceMap.getOrElse(plaintext.charAt(i), 0.0)
      makePlaintextScore(i+1, score + letterScore, plaintext)
    }
  }

  def scorePlaintext(plaintext: String): Double = {
    // Make all letters lowercase for easier letter frequency analysis
    val loweredCase = plaintext.toLowerCase()

    makePlaintextScore(0, 0.0, loweredCase)
  }

  def convertDecimalArrayToPlaintextArray(decimalArray: Array[Int]): Array[Char] = {
    decimalArray
      .map(_.toChar)
  }

  // Decode a hex string to a plaintext string using a key
  def decodeHexStringWithKey(hexStr: String, key: Int): String = {
    C1.splitStringIntoArray(hexStr, 2)
      .map(Integer.parseInt(_, 16))
      .map(_ ^ key)
      .map(_.toChar)
      .mkString("")
  }

  @tailrec
  final def findBestKey(decimalArray: Array[Int], i: Int, bestScore: Double, bestKey: Int): (Int, Double) = {
    if (i == 256){
      (bestKey, bestScore)
    }
    else {
      // XOR with the decimal i
      val xored = decimalArray.map(_ ^ i)
      // Make a plaintext string from the XORed decimal array
      val plaintext = convertDecimalArrayToPlaintextArray(xored).mkString("")

      // Get a new letter frequency score using the plaintext and the current key i
      val currScore = scorePlaintext(plaintext)

      // Call findBestKey again with currScore if it's better (smaller) than the current best score
      if (currScore > bestScore){
        findBestKey(decimalArray, i+1, currScore, i)
      }
      // Otherwise, call the function again using bestScore
      else {
        findBestKey(decimalArray, i+1, bestScore, bestKey)
      }
    }
  }

  def breakHexCipherSingleByteXORCipher(hexStr: String): Int = {
    // Get the an array containing the decimal conversions of each hex digit
    val hexArray = C1.splitStringIntoArray(hexStr, 2)
    val decimalArray = C2.convertHexArrayToDecimalArray(hexArray)

    // Try xoring with each possible character and score the plaint text
    val (bestKey, bestScore) = findBestKey(decimalArray, 0, Double.MinValue, 0)
    bestKey
  }

  def solution(hexStr: String): Int = {
    breakHexCipherSingleByteXORCipher(hexStr)
  }
}
