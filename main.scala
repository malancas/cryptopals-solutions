import s1q1.convertHexStringtoBase64String
import s1q2.getFixedXORofHexBuffers
import s1q3.decodeHexString

object MainModel {
  def main(args: Array[String]): Unit = {
    // Set 1
    // Question 1
    var sampleInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    var expectedResult = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    var actualResult = convertHexStringtoBase64String(sampleInput)
    assert(expectedResult == actualResult)

    // Question 2
    var sampleBuffer0 = "1c0111001f010100061a024b53535009181c"
    var sampleBuffer1 = "686974207468652062756c6c277320657965"
    expectedResult = "746865206b696420646f6e277420706c6179"

    actualResult = getFixedXORofHexBuffers(sampleBuffer0, sampleBuffer1)
    assert(expectedResult == actualResult)

    // Question 3
    var samppleHexString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    actualResult = decodeHexString(samppleHexString)
  }
}
