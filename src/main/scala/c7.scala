package c7

import javax.crypto._
import c6.C6

// http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
class C7 {
  val c6 = new C6

  // val encryptedTextMatrix = DenseMatrix(4, 4, encryptedTextBlock)
  // val roundKeyMatrix = DenseMatrix(4, 4, roundKey)

  /*
  For each row in the text block, shift 1 more to the left
  than in the above row such that the first row shifts 0 times
  the second row shifts 1 time, the third shifts 2 two times, etc.
  */
  /*
  def shiftRows(encryptedTextBlock: DenseMatrix[String]): DenseMatrix[String] = {

  }

  
  -Each column of the encrypted text is xored with the 
   corresponding row of the round key
  -The result column replaces the original encrypted column
  -Repeat for all columns in the encrypted text
  
  def addRoundKey(encryptedTextBlock: DenseMatrix[String], roundKey: DenseMatrix[String]): DenseMatrix[String] = {
    encryptedTextMatrix.zip(roundKeyMatrix)
    val newEncryptedMatrix = encryptedTextMatrix(::, *).map( dv => dv._1 ^ dv._2)
    newEncryptedMatrix
  }

  def doAllRounds(): String = {
    // Initial round, only use roundKey
  }

  def implementECB(ciphertext: String , key: String): Unit = {
      // Ciphertext blocks and key should contain the same number of bytes
  }

  def aesAlgorithm(key: String): Unit = {
    // Create state (a 4x4 matrix of the bytes stored in the original key)

    // There are four steps
    // 1. KeyExpansions - Round keys are derived from cipher key using Rijndael's key scheduler
    // 2. 
  }
  */

  /*
  def generateRijndaelSBox(i: Int, s: Unsigned Int, result: Int): = {
    if (i == 5){
      99 ^ result
    }
    else {
      val xored = result ^ s
      val xoredBinary = c6.toBinary(xored)
      val rotated = xoredBinary.slice(1) + xoredBinary(0)
      rijndaelSBox(i+1, rotated, xored)
    }
  }
  */

  /*
  def keyScheduleCore(temporaryKey: Array[String], rconIteration: i): String = {
    // Copy the input to the output
    val output = temporaryKey

    // Rotate the output 8 bits to the left
    // The left most hex digit is rotated to the end of the string
    val output2 = temporaryKey.slice(1) + temporaryKey(0)

    // Apply Rijndael's S-box on all four individual bytes in the output word
    // https://en.wikipedia.org/wiki/Rijndael_S-box

    "stuff"
  }
  */

  /*
  def rijndaelKeyScheduler(key: String): Unit = {
    // Step 1: First 16 characters of the expanded key are the original key
    val newKey = key
    // Step 2
    val rconIteration = 1
    // Step 3
    val n = 16
    val i = 0
    for (i <- 0 to 176) {
      // Create 4 more bytes of expanded key
      
      // 1. Create 4 btye long temporary variables
      val t = 0000

      // 2. We assign the value of the previous four bytes in the expanded key to t
      val t2 = 0000+n.slice(n.length-4, n.length)

      // 3. We perform the key schedule core (see above) on t, with i as the rcon iteration value
    }
  }
  */

  /*
  def decrypt(ciphertext: String, key: String): String = {
    // Create key and cipher
    val aesKey = new SecretKeySpec(key.getBytes(), "AES")
    val cipher = Cipher.getInstance("AES")

    // Decrypt the text
    cipher.init(Cipher.DECRYPT_MODE, aesKey);
    val decrypted = new String(cipher.doFinal(ciphertext));

    decrypted
  }

  def decryptAES128ViaECB(): Unit = {
    // Get content from the file
    val fileLines = c6.getLinesFromFile("7.txt")

    // Create a list of the content, where each element
    // is a character
    val base64CiphertextList = fileLines.mkString("").toList

    // Convert the base 64 encoded text from the file to a binary string
    val binaryCiphertext = base64CiphertextList.map(c6.convertBase64DigitToSixDigitBinaryString(_)).mkString("")

    val plaintext = decrypt(binaryCiphertext)
    println(plaintext)
  }
  */

  def solution(): String = {
    "nothing yet"
  }
}