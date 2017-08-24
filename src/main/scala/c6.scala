package c6

import scala.io.Source
import scala.collection.mutable.PriorityQueue
import c1.C1
import c3.C3
import c5.C5

class C6 {
  def toBinary(i: Int): String = {
    // Append zeroes to the binary string so it's eight characters long
    val binaryString = i.toBinaryString
    "0" * (8 - binaryString.length) + binaryString
  }

  // Count the number of differing bits between two binary strings
  def getHammingDistanceBetweenBinaryStrings(bStr0: String, bStr1: String): Int = {
    bStr0.zip(bStr1).count(c => c._1 != c._2)
  }

  def getHammingDistanceBetweenText(plaintext0: String, plaintext1: String): Int = {
    // Convert the plaintexts to binary string arrays
    val binaryStr0 = plaintext0.map(toBinary(_))
    val binaryStr1 = plaintext1.map(toBinary(_))

    // Get the sum of the hamming distance between each element of the arrays
    binaryStr0.zip(binaryStr1).map(c => getHammingDistanceBetweenBinaryStrings(c._1, c._2)).sum
  }

  def getNormalizedHammingDistanceBetweenText(str0: String, str1: String, keySize: Int): Double = {
    // Divide the hamming distance by keySize to normalize it
    getHammingDistanceBetweenText(str0, str1).toDouble / keySize
  }

  def getNormalizedHammingDistanceBetweenBinaryStrings(bin0: String, bin1: String, keySize: Int): Double = {
    getHammingDistanceBetweenBinaryStrings(bin0, bin1)
  }

  def getThreeBestKeySizes(keySize: Int, binaryCiphertext: String, smallestHammingDistances: PriorityQueue[(Double, Int)]): PriorityQueue[(Double, Int)] = {
    if (keySize == 41 || binaryCiphertext.length < keySize * 2){
      smallestHammingDistances
    }
    else {
      // Get the first two substrings of length keySize
      //val plaintext0 = plaintext.substring(0, keySize)
      //val plaintext1 = plaintext.substring(keySize, (keySize * 2))
      val binarySubString0 = binaryCiphertext.substring(0, keySize * 8)
      val binarySubString1 = binaryCiphertext.substring(keySize * 8, (keySize * 8 * 2))

      val hamDistance = getNormalizedHammingDistanceBetweenBinaryStrings(binarySubString0, binarySubString1, keySize)

      if (smallestHammingDistances.length < 3){
        val newArray = smallestHammingDistances ++ PriorityQueue((hamDistance, keySize))
        getThreeBestKeySizes(keySize + 1, binaryCiphertext, newArray)
      }
      else if (hamDistance < smallestHammingDistances.head._1){
        smallestHammingDistances.dequeue
        val newArray = smallestHammingDistances ++ PriorityQueue((hamDistance, keySize))
        getThreeBestKeySizes(keySize + 1, binaryCiphertext, newArray)
      }
      else {
        getThreeBestKeySizes(keySize + 1, binaryCiphertext, smallestHammingDistances)
      }
    }
  }

  def getRepeatingKeyXORWithChosenKeySize(keySize: Int, binaryCiphertext: String): String = {
    // For each key
      // 1. Solve each transposed block like a single character XOR
      // 2. Find the correct key for each block
      // 3. Put these keys together to get the correct key and decrypt the file
      // 4. Get the score for each decrypted file text
      // 5. Return the plain text with the best score

    val c1 = new C1
    // Separate the plaintext into blocks of keySize length
    // Seperate each binary number and convert them into decimal equivalents
    val decimalCiphertext = c1.splitStringIntoArray(binaryCiphertext, 8).map(Integer.parseInt(_, 2)).mkString("")

    // Group the numbers into keySize length chunks
    val textBlocks = decimalCiphertext.grouped(keySize).toArray.map(c1.splitStringIntoArray(_, 1))

    // Transpose blocks. Make a block that is the first byte of every block, another block that is every second byte, etc
    val transposedBlocks = textBlocks.transpose.map(_.mkString(""))

    // Solve each block like it is a single character XOR
    val c3 = new C3
    val repeatingXORKey = transposedBlocks.map(c3.breakSingleByteXORCipher_noHex(_)).mkString("")

    repeatingXORKey
  }


  // Get the keys that correspond with the best key sizes
  /*
   * def getTheRepeatingXORKey(bestKeySizes: List[(Double, Int)], plaintext: String): Array[String] = {
   *  bestKeySizes match {
   *   case h :: t => {
   *     val key = breakRepeatingKeyXORWithChosenKeySize(h._1.toInt, plaintext)
   *      Array(key) ++ getTheRepeatingXORKeys2(t, plaintext)
   *   }
   *   case _ => Array[String]()
   * }
   *}
   */


  def getLinesFromFile(): List[String] = {
    Source
      .fromResource("6.txt")
      .getLines
      .toList
  }

  def convertBase64DigitToBinaryString(base64Digit: Char): String = {
    val decimalEquivalent = base64Digit.toInt
    if (65 <= decimalEquivalent && decimalEquivalent <= 90) {
      (decimalEquivalent - 65).toBinaryString
    }
    else if (97 <= decimalEquivalent && decimalEquivalent <= 122) {
      (decimalEquivalent - 71).toBinaryString
    }
    else if (48 <= decimalEquivalent && decimalEquivalent <= 57) {
      (decimalEquivalent + 4).toBinaryString
    }
    else if (decimalEquivalent == 43){
      (decimalEquivalent + 19).toBinaryString
    }
    else { // decimalEquivalent == 47
      (decimalEquivalent + 16).toBinaryString
    }
  }

  def convertBase64DigitToSixDigitBinaryString(base64Digit: Char): String = {
    val binaryString = convertBase64DigitToBinaryString(base64Digit)
    "0" * (6 - binaryString.length) + binaryString
  }

  def decryptWithAllKeys(binaryCiphertext: String, keys: List[(Double, Int)]): Unit = {
    keys match {
      case h :: t => {
        // Decrypt using only one key size
        val repeatingXORKey = getRepeatingKeyXORWithChosenKeySize(h._2, binaryCiphertext)

        // Convert binary ciphertext to its decimal equivalent
        val c = new C1
        val decimalCiphertext = c
          .splitStringIntoArray(binaryCiphertext, 8)
          .map(Integer.parseInt(_, 2))
          .mkString("")

        // Decode the file contents with the key
        val c5 = new C5
        val decryptedText = c5.encodeStringWithRepeatingKeyXOR(decimalCiphertext, repeatingXORKey)
        println(s"DECRYPTED: $decryptedText")

        // Score the decrypted text
        val c3 = new C3
        val score = c3.scorePlaintext(decryptedText)

        decryptWithAllKeys(binaryCiphertext, t)
      }
      case Nil => "Done"
    }
  }

  def decryptFile(): Unit = {
    // Form a list of the file contents
    val fileLines = getLinesFromFile()

    // Form a single string made up of the file contents
    val base64CiphertextList = fileLines.mkString("").toList

    // Convert the base 64 encoded text from the file to a binary string
    val binaryCiphertext = base64CiphertextList.map(convertBase64DigitToSixDigitBinaryString(_)).mkString("")

    // Decode the binary string to plaintext
    val c = new C1
    /*
    val plaintext = c
      .splitStringIntoArray(binaryCiphertext, 8)
      .map(Integer.parseInt(_, 2))
      .map(_.toChar)
      .mkString("")
    */
    
    // Convert binary ciphertext to its decimal equivalent
    val decimalCiphertext = c
      .splitStringIntoArray(binaryCiphertext, 8)
      .map(Integer.parseInt(_, 2))
      .mkString("")

    // Get best key sizes
    val bestKeySizes = getThreeBestKeySizes(2, binaryCiphertext, PriorityQueue[(Double, Int)]()).toList

    decryptWithAllKeys(binaryCiphertext, bestKeySizes)

    /*
    // Decrypt using only one key size
    val repeatingXORKey = getRepeatingKeyXORWithChosenKeySize(bestKeySizes.head._2, binaryCiphertext)
    println(s"repeating key: $repeatingXORKey")

    // Decode the file contents with the key
    val c5 = new C5
    val decryptedText = c5.encodeStringWithRepeatingKeyXOR(decimalCiphertext, repeatingXORKey)
    println(decryptedText)

    // Score the decrypted text
    val c3 = new C3
    val score = c3.scorePlaintext(decryptedText)
    */
  }
}
