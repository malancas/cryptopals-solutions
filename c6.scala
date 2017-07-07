package c6

/*
 Guess key sizes between 2 and 40
 */

class C6 {
  def toBinary(i: Int): String = {
    val initialBin = i.toBinaryString
    val diff = 8 - initialBin.length
    "0" * diff + initialBin
  }

  def getDistanceForEachChar(bStr0: String, bStr1: String): Int = {
    bStr0.zip(bStr1).count(c => c._1 != c._2)
  }

  def getHammingDistance(str0: String, str1: String): Int = {
    //str0.zip(str1).count(c => c._1 != c._2)
    val binaryStr0 = str0.map(toBinary(_))
    val binaryStr1 = str1.map(toBinary(_))

    binaryStr0.zip(binaryStr1).map(c => getDistanceForEachChar(c._1, c._2)).sum
  }

  def getNormalizedHammingDistance(str0: String, str1: String, keySize: Int): Double = {
    getHammingDistance(str0, str1).toDouble / keySize
  }

  def getThreeBestKeySizes(keySize: Int, str0: String, str1: String, smallestArray: Array[Double]): Array[Double] = {
    if (keySize == 41){
      smallestArray
    }
    else {
      val hamDistance = getNormalizedHammingDistance(str0, str1, keySize)

      if (smallestArray.length < 3){
        val newArray = (smallestArray ++ Array(hamDistance)).sorted
        getThreeBestKeySizes(keySize + 1, str0, str1, newArray)
      }
      else if (hamDistance < smallestArray(2)){
        val newArray = (smallestArray.slice(0,2) ++ Array(hamDistance)).sorted
        getThreeBestKeySizes(keySize + 1, str0, str1, newArray)
      }
      else {
        getThreeBestKeySizes(keySize + 1, str0, str1, smallestArray)
      }
    }
  }

  def decryptFile(): String = {
    "nothing yet"
  }
}
