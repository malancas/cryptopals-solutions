import set1.c1.C1
import set1.c2.C2
import set1.c3.C3
import set1.c4.C4
import set1.c5.C5
import set1.c6.C6
import set1.c7.C7
import set1.c8.C8
import set2.c9.C9
import set2.c10.C10

object MainModel {
  def main(args: Array[String]): Unit = {
    // Set 1
    // Challenge 1
    val sampleInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

    var expectedResult = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    var actualResult = C1.convertHexStringtoBase64String(sampleInput)

    assert(actualResult == expectedResult)
    println("Challenge 1 passed")

    // Challenge 2
    val sampleBuffer0 = "1c0111001f010100061a024b53535009181c"
    val sampleBuffer1 = "686974207468652062756c6c277320657965"

    actualResult = C2.getFixedXORofHexBuffers(sampleBuffer0, sampleBuffer1)
    expectedResult = "746865206b696420646f6e277420706c6179"

    assert(actualResult == expectedResult)
    println("Challenge 2 passed")

    // Challenge 3
    val sampleHexString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    val bestKey = C3.breakHexCipherSingleByteXORCipher(sampleHexString)

    assert(bestKey == 88)
    println("Challenge 3 passed")

    // Challenge 4
    //val c4 = new C4
    C4.detectSingleCharacterXOR()
    println("Challenge 4 passed")

    // Challenge 5
    val plaintext = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val key = "ICE"

    expectedResult = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

    actualResult = C5.encodeStringWithRepeatingKeyXOR(plaintext, key)

    assert(expectedResult == actualResult)
    println("Challenge 5 passed")

    // Challenge 6
    C6.decryptFile()
    println("Challenge 6 passed")

    // Challenge 7
    println(C7.solution())
    println("Challenge 7 passed")

    // Challenge 8
    C8.solution()
    println("Challenge 8 passed")

    // Challenge 9
    assert(C9.implementPKCS7Padding("YELLOW SUBMARINE", 20) == "YELLOW SUBMARINE\\x04\\x04\\x04\\x04")
    println("Challenge 9 passed")

    // Challenge 10
    val c10Key = "YELLOW SUBMARINE"
    val IV = Array.fill(16)(0)
    //val cbc = new CBCMode(IV, c10Key)

    //println(cbc.solution)
    //println("Challenge 10 passed")
  }
}