package c6

import scala.util.Sorting.stableSort
import scala.collection.mutable.PriorityQueue
import c1.C1
import c3.C3

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

  def getThreeBestKeySizes(keySize: Int, plaintext: String, smallestHammingDistances: Array[(Double, Int)]): Array[(Double, Int)] = {
    if (keySize == 41){
      smallestHammingDistances
    }
    else {
      // Get the first two substrings of length keySize
      val plaintext0 = plaintext.substring(0, keySize)
      val plaintext1 = plaintext.substring(keySize, (keySize * 2))

      val hamDistance = getNormalizedHammingDistanceBetweenText(plaintext0, plaintext1, keySize)

      if (smallestHammingDistances.length < 3){
        val newArray = (smallestHammingDistances ++ Array(hamDistance, keySize)).sorted
        getThreeBestKeySizes(keySize + 1, plaintext0, plaintext1, newArray)
      }
      else if (hamDistance < smallestHammingDistances(2)(0)){
        val newArray = (smallestHammingDistances.slice(0,2) ++ Array(hamDistance, keySize)).sorted
        getThreeBestKeySizes(keySize + 1, plaintext0, plaintext1, newArray)
      }
      else {
        getThreeBestKeySizes(keySize + 1, plaintext0, plaintext1, smallestHammingDistances)
      }
    }
  }

  def breakRepeatingKeyXORWithChosenKeySize(keySize: Int, plaintext: String): String = {
    val c1 = new C1
    // Separate the plaintext into blocks of keySize length
    val textBlocks = plaintext.grouped(keySize).toArray.map(c1.splitStringIntoArray(_))

    // Transpose blocks. Make a block that is the first byte of every block, another block that is every second byte, etc.
    val transposedBlocks = textBlocks.transpose.map(_.mkString(""))

    // Solve each block like it is a single character XOR
    val c3 = new C3
    transposedBlocks.map(c3.breakSingleByteXORCipher(_))
  }

  def getTheRepeatingXORKeys(i: Int, bestKeySizes: Array[(Double, Int)], plaintext:String, keys: Array[String]): Array[String] = {
    breakRepeatingKeyXORWithChosenKeySize()
  }

  def getTheRepeatingXORKeys2(bestKeySizes: List[(Double, Int)], plaintext: String): Array[String] = {
    bestKeySizes match {
      case h :: t => {
        val key = breakRepeatingKeyXORWithChosenKey(h._1, plaintext)
        Array(key) ++ getTheRepeatingXORKeys2(t, plaintext)
      }
      case _ => Array[String]()
    }
  }

  def breakRepeatingKeyXOR(keySize: Int, plaintext:String): String = {
    // Get the three most probable keys
    val bestKeySizes = getThreeBestKeySizes(2, plaintext, Array[(Double, Int)]())


    // Get the keys
    getTheRepeatingXORKeys2(bestKeySizes.toList, plaintext)

    "nothing yet"
  }
}
