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

  def getDistanceForEachLetter(bStr0: String, bStr1: String): Int = {
    println(bStr0)
    println(bStr1)
    bStr0.zip(bStr1).count(c => c._1 != c._2)
  }

  def getHammingDistanceBetweenStrings_aux(nums: IndexedSeq[Int], sum: Int, i: Int): Int = {
    if (i == nums.length) {
      sum
    }
    else {
      getHammingDistanceBetweenStrings_aux(nums, sum + nums(i), i+1)
    }
  }

  def getHammingDistanceBetweenStrings(str0: String, str1: String): Int = {
    //str0.zip(str1).count(c => c._1 != c._2)
    val binaryStr0 = str0.map(toBinary(_))
    val binaryStr1 = str1.map(toBinary(_))

    val nums = binaryStr0.zip(binaryStr1).map(c => getDistanceForEachLetter(c._1, c._2))
    getHammingDistanceBetweenStrings_aux(nums, 0, 0)
  }

  def decryptFile(): String = {
    "nothing yet"
  }
}
