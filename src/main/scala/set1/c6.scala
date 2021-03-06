package set1.c6

import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.math.Ordering
import scala.util.Sorting
import set1.c1.C1
import set1.c3.C3
import set1.c4.C4
import set1.c5.C5

object C6 {
  case class HammingDistKeySizeCount(hammingDistance: Double, keySizes: Array[Int]) {
    val data = (hammingDistance, keySizes)
    def getHammingDistance() = data._1
    def getKeySizes() = data._2
  }

  def hamDistOrder(h: HammingDistKeySizeCount) = h.data._1

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
    getHammingDistanceBetweenText(str0, str1) / keySize  
  }

  def getNormalizedHammingDistanceBetweenBinaryStrings(bin0: String, bin1: String, keySize: Int): Double = {
    getHammingDistanceBetweenBinaryStrings(bin0, bin1)
  }

  @tailrec
  final def getThreeBestKeySizes(keySize: Int, binaryCiphertext: String, characterCiphertext: String,
    smallestHammingDistances: PriorityQueue[Double], hammingDistMap: Map[Double, Array[Int]]): Array[Int] = {
    if (keySize == 41 || binaryCiphertext.length < keySize * 2){
      // Copy the three smallest sizes to an array
      smallestHammingDistances.toArray.slice(0,3)

      // Get the corresponding key sizes from the map and return them
      // in an array
      smallestHammingDistances.map(hammingDistMap(_)).toArray.flatten
    }
    else {
      // Get the first two substrings of length keySize
      val cipherSubstring0 = characterCiphertext.substring(0, keySize)
      val cipherSubstring1 = characterCiphertext.substring(keySize, keySize * 2)

      val hamDistance = getNormalizedHammingDistanceBetweenText(cipherSubstring0, cipherSubstring1, keySize)
      if (hammingDistMap.contains(hamDistance)) {
        // Update the corresponding array to include the current key size
        val newKeySizeArray = hammingDistMap(hamDistance) ++ Array(keySize)
        val tempMap = hammingDistMap - (hamDistance)
        getThreeBestKeySizes(keySize + 1, binaryCiphertext, characterCiphertext, smallestHammingDistances, tempMap ++ HashMap(hamDistance -> newKeySizeArray))
      }
      else {
        smallestHammingDistances.enqueue(hamDistance)
        getThreeBestKeySizes(keySize + 1, binaryCiphertext, characterCiphertext, smallestHammingDistances, hammingDistMap ++ HashMap(hamDistance -> Array(keySize)))        
      }
    }
  }

  def getRepeatingKeyXORWithChosenKeySize(keySize: Int, binaryCiphertext: String): String = {
    // Seperate each binary number and convert them into decimal equivalents
    val decimalCipherArray = C1.splitStringIntoArray(binaryCiphertext, 8).map(Integer.parseInt(_, 2))
    val decimalCiphertext = C1.splitStringIntoArray(binaryCiphertext, 8).map(Integer.parseInt(_, 2)).mkString("")

    // Group the numbers into keySize length chunks
    val keySizeGroupedTextBlocks = decimalCipherArray.grouped(keySize).toArray
    val hexTextBlocks = keySizeGroupedTextBlocks.map(_.map(_.toInt)).map(C5.convertDecimalArrayToHexString(0, _, ""))
    val hexArrayTextBlocks = hexTextBlocks.map(C1.splitStringIntoArray(_, 2))

    // Transpose blocks. Make a block that is the first byte of every block, another block that is every second byte, etc
    val transposedBlocks = hexArrayTextBlocks.transpose.map(_.mkString(""))

    // Solve each block like it is a single character XOR
    val repeatingXORKey = transposedBlocks.map(C3.breakHexCipherSingleByteXORCipher(_).toChar).mkString("") 
    
    repeatingXORKey
  }

  def decryptWithAllKeys(binaryCiphertext: String, keys: List[Int], currBestDecryptedText: String, currBestScore: Double): String = {
    keys match {
      case h :: t => {
        // Decrypt using only one key size
        val repeatingXORKey = getRepeatingKeyXORWithChosenKeySize(h, binaryCiphertext)

        // Convert binary ciphertext to its decimal equivalent
        val characterCiphertext = C1
          .splitStringIntoArray(binaryCiphertext, 8)
          .map(Integer.parseInt(_, 2).toChar)
          .mkString("")

        //println(s"ciphertext: $characterCiphertext")

        val charKey = C1
          .splitStringIntoArray(repeatingXORKey, 1)

        // Decode the file contents with the key
        val keyEncodedText = C5.encodeStringWithRepeatingKeyXOR(characterCiphertext, repeatingXORKey)
        val decryptedText = C1.splitStringIntoArray(keyEncodedText, 2).map(Integer.parseInt(_, 16).toChar).mkString("")

        // Score the decrypted text
        val score = C3.scorePlaintext(keyEncodedText)
        val keysize = h
        if (score < currBestScore){
          decryptWithAllKeys(binaryCiphertext, t, decryptedText, score)
        }
        else {
          decryptWithAllKeys(binaryCiphertext, t, currBestDecryptedText, currBestScore)          
        }
      }
      case Nil => {
        currBestDecryptedText
      }
    }
  }

  def decryptFile(): Unit = {
    // Form a list of the file contents
    val fileLines = C4.getLinesFromFile("6.txt")

    // Form a single string made up of the file contents
    val base64CiphertextList = fileLines.mkString("").toList

    // Convert the base 64 encoded text from the file to a binary string
    val binaryCiphertext = base64CiphertextList.map(convertBase64DigitToSixDigitBinaryString(_)).mkString("")

    // Create min heap to keep track of the smallest hamming distances
    val minHeap = scala.collection.mutable.PriorityQueue.empty(Ordering[Double]).reverse

    // A hash map used to keep track of which key sizes produce which hamming distances
    val hamDistMap = HashMap[Double, Array[Int]]()

    // Get the three best key sizes
    val characterCiphertext = C1
      .splitStringIntoArray(binaryCiphertext, 8)
      .map(Integer.parseInt(_, 2).toChar)
      .mkString("")
      
    val bestKeySizes = getThreeBestKeySizes(2, binaryCiphertext, characterCiphertext, minHeap, hamDistMap).toList
    
    val decryptedText = decryptWithAllKeys(binaryCiphertext, bestKeySizes, "", Double.MaxValue)
  }

  def solution(): Unit = {
    decryptFile()
  }
}
