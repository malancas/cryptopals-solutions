import c1.C1
import c2.C2
import c3.C3
import c4.C4
import c5.C5

object MainModel {
  def main(args: Array[String]): Unit = {
    // Set 1
    // Challenge 1
    val c1 = new C1
    val sampleInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

    var expectedResult = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    var actualResult = c1.convertHexStringtoBase64String(sampleInput)
    println(s"Actual result: $actualResult")
    println(s"Expected result: $expectedResult")
    assert(actualResult == expectedResult)

    // Challenge 2
    val c2 = new C2

    val sampleBuffer0 = "1c0111001f010100061a024b53535009181c"
    val sampleBuffer1 = "686974207468652062756c6c277320657965"

    actualResult = c2.getFixedXORofHexBuffers(sampleBuffer0, sampleBuffer1)
    expectedResult = "746865206b696420646f6e277420706c6179"

    println(s"Actual: $actualResult")
    println(s"Expect: $expectedResult")
    assert(actualResult == expectedResult)

    // Challenge 3
    val c3 = new C3
    val sampleHexString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    val bestKey = c3.breakSingleByteXORCipher(sampleHexString)
    println(s"Best key: $bestKey")
    assert(bestKey == 88)

    // Challenge 4
    val c4 = new C4
    c4.detectSingleCharacterXOR()

    // Challenge 5
    val plaintext0 = "Burning 'em, if you ain't quick and nimble"
    val plaintext1 = "I go crazy when I hear a cymbal"
    val key = "ICE"

    var expectedResult0 = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
    var expectedResult1 = "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

    val c5 = new C5

    var actualResult0 = encodeStringWithRepeatingKeyXOR(plaintext0, key)
    var actualResult1 = encodeStringWithRepeatingKeyXOR(plaintext1, key)

    assert(expectedResult0 == actualResult0)
    assert(expectedResult1 == actualResult1)
  }
}
