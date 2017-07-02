import c11.C11
import c12.C12

object MainModel {
  def main(args: Array[String]): Unit = {
    // Set 1
    // Question 1
    val c1 = new C11
    val sampleInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

    var expectedResult = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    var actualResult = c1.convertHexStringtoBase64String(sampleInput)
    println(s"Actual result: $actualResult")
    println(s"Expected result: $expectedResult")
    assert(actualResult == expectedResult)

    
    // Question 2
    val c2 = new C12

    val sampleBuffer0 = "1c0111001f010100061a024b53535009181c"
    val sampleBuffer1 = "686974207468652062756c6c277320657965"

    actualResult = c2.getFixedXORofHexBuffers(sampleBuffer0, sampleBuffer1)
    expectedResult = "746865206b696420646f6e277420706c6179"

    println(s"Actual: $actualResult")
    println(s"Expect: $expectedResult")
    assert(actualResult == expectedResult)

    // Question 3
    val samppleHexString = "1b37373331363f78151b7f2b783431333d78397828372d63c78373e783a393b3736"
  }
}
