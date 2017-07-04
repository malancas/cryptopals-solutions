package c3

import c2.C2

class C3 {
  // Letter frequencies sourced from: https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
  val referenceMap = Map('e' -> 12.02, 't' -> 9.10, 'a' -> 8.12, 'o' -> 7.68, 'i' -> 7.31, 'n' -> 6.95,
    's' -> 6.28, 'r' -> 6.02, 'h' -> 5.92, 'd' -> 4.32, 'l' -> 3.89, 'u' -> 2.88, 'c' -> 2.71, 'm' -> 2.61,
    'f' -> 2.30, 'y' -> 2.11, 'w' -> 2.09, 'g' -> 2.03, 'p' -> 1.82, 'b' -> 1.49, 'v' -> 1.11, 'k' -> 0.69,
    'x' -> 0.17, 'q' -> 0.11, 'j' -> 0.10, 'z' -> 0.07)

  def makePlaintextScore(i: Int, score: Double, frequencyMap: Map[Char, Double]): Double = {
    if (i == 256) { score }
    else {
      val diff = math.abs(frequencyMap.getOrElse(i.toChar, 0.0) - referenceMap.getOrElse(i.toChar, 0.0))
      makePlaintextScore(i+1, score + diff, frequencyMap)
    }
  }

  def scorePlaintext(plaintext: String): Double = {
    // Make all letters lowercase for easier letter frequency analysis
    val loweredCase = plaintext.toLowerCase()

    // Create a map of frequency values by mapping each letter that appears in the plaintext string
    // to the number of times it appears over the length of the string itself
    val frequencyMap = plaintext.groupBy(c => c).mapValues(_.length.toDouble / plaintext.length)

    makePlaintextScore(0, 0.0, frequencyMap)
  }

  def convertDecimalArrayToPlaintextArray(decimalArray: Array[Int]): Array[Char] = {
    decimalArray
      .map(_.toChar)
  }

  def findBestKey(decimalArray: Array[Int], i: Int, bestScore: Double, bestKey: Int): (Int, Double) = {
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
      if (currScore < bestScore){
        findBestKey(decimalArray, i+1, currScore, i)
      }
      // Otherwise, call the function again using bestScore
      else {
        findBestKey(decimalArray, i+1, bestScore, bestKey)
      }
    }
  }

  def breakSingleByteXORCipher(hexStr: String): Int = {
    // Get the an array containing the decimal conversions of each hex digit
    val c = new C2
    val decimalArray = c.convertHexArrayToDecimalArray(hexStr.split("(?<=\\G..)"))

    // Try xoring with each possible character and score the plaint text
    val (bestKey, bestScore) = findBestKey(decimalArray, 0, Double.MaxValue, 0)
    bestKey
  }
}
