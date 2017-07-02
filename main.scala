import c11.C11

object MainModel {
  def main(args: Array[String]): Unit = {
    // Set 1
    // Question 1
    var c = new C11
    val sampleInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

    val expectedResult = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    val actualResult = c.convertHexStringtoBase64String(sampleInput)
    println(s"Actual result: $actualResult")
    println(s"Expected result: $expectedResult")
    assert(actualResult == expectedResult)

    /*
    // Question 2
    val sampleBuffer0 = "1c0111001f010100061a024b53535009181c"
    val sampleBuffer1 = "686974207468652062756c6c277320657965"
    expectedResult = "746865206b696420646f6e277420706c6179"

    // Question 3
    val samppleHexString = "1b37373331363f78151b7f2b783431333d78397828372d
    63c78373e783a393b3736"
    */
  }
}
