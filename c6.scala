package c6

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

  def getThreeBestKeySizes(keySize: Int, plaintext0: String, plaintext1: String, smallestHammingDistances: Array[Double]): Array[Double] = {
    if (keySize == 41){
      smallestHammingDistances
    }
    else {
      val hamDistance = getNormalizedHammingDistanceBetweenText(plaintext0, plaintext1, keySize)

      if (smallestHammingDistances.length < 3){
        val newArray = (smallestHammingDistances ++ Array(hamDistance)).sorted
        getThreeBestKeySizes(keySize + 1, plaintext0, plaintext1, newArray)
      }
      else if (hamDistance < smallestHammingDistances(2)){
        val newArray = (smallestHammingDistances.slice(0,2) ++ Array(hamDistance)).sorted
        getThreeBestKeySizes(keySize + 1, plaintext0, plaintext1, newArray)
      }
      else {
        getThreeBestKeySizes(keySize + 1, plaintext0, plaintext1, smallestHammingDistances)
      }
    }
  }

  def decryptFile(keySize: Int, plaintext:String): String = {
    "nothing yet"
  }
}
