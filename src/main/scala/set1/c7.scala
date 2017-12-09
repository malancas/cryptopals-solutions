package set1.c7

import javax.crypto.spec.SecretKeySpec
import javax.crypto.Cipher
import scala.io.Source
import java.util.Base64

import set1.AES128Matrix

// http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf

object C7 {
  // The simple JavaX solution
  def javaxDecrypt(encryptedText: String, secret: String): String = {
    val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), "AES")
    val cipher = Cipher.getInstance("AES/ECB/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, secretKey)
    new String(cipher.doFinal(Base64.getDecoder.decode(encryptedText)))  
  }

  def javaxSolution(): String = {
    val encryptedText = Source.fromResource("7.txt").getLines.mkString("")
    val key = "YELLOW SUBMARINE"
    decrypt(encryptedText, key)
  }

  // ECB implementation

  def divideTextIntoBlocks(text: String): List[Array[Byte]] = {
    // Assume block size is 16 bytes for now
    text.getBytes.grouped(16).map(_.toArray).toList
  }

  def processTextBlocks(textBlocks: List[Array[Byte]], cipherBlock: Array[Int], encryptedText: String): String = {
    textBlocks match {
      case h :: t => {
        // Process the first block
        val encryptedBlock = encryptBlock(h, cipherBlock)
        val encryptedTextPortion = encryptedBlock.map(_.toChar).mkString("")
        processTextBlocks(t, encryptedBlock, encryptedText + encryptedTextPortion)
      }
      case Nil => {
        encryptedText
      }
    }
  }

  def encryptBlock(textBlock: Array[Byte], cipherBlock: Array[Int]): Array[Int] = {
    // XOR the text block with the previous ciphertext block
    val xored = textBlock.zip(cipherBlock).map { case (x,y) =>  x ^ y }

    // Encrypted the result
    val keyArray = key.getBytes()
    val encrypted = xored.zip(keyArray).map { case (x,y) => (x ^ y)}
    encrypted
  }

  def ECBMode(plaintext: String, key: String): String = {
    val textMatrix = new AES128Matrix(plaintext)
    val keyMatrix = new AES128Matrix(key)

    val textBlocks = divideTextIntoBlocks(plaintext)
    // Begin with encrypting the first textblock with the IV
    val initialCipherBlock = encryptBlock(textBlocks.head, IV)

    // Now use the initial cipher block and encrypt the remaining text blocks
    val encryptedText = processTextBlocks(textBlocks.tail, initialCipherBlock, key)
    encryptedText
  }

  def solution(): String = {
    val encryptedText = Source.fromResource("7.txt").getLines.mkString("")
    ECBMode(encryptedText, "YELLOW SUBMARINE")
  }
  
  // AES-128 implementation

  /*
  -Each column of the encrypted text is xored with the corresponding row of the round key
    - Convert text from single string representation into a matrix representation
    - process each column
  -The result column replaces the original encrypted column
  -Repeat for all columns in the encrypted text
  */

  def addRoundKey(encryptedTextBlock: DenseMatrix[String], roundKey: DenseMatrix[String]): DenseMatrix[String] = {
    encryptedTextMatrix.zip(roundKeyMatrix)
    val newEncryptedMatrix = encryptedTextMatrix(::, *).map( dv => dv._1 ^ dv._2)
    newEncryptedMatrix
  }

  def doAllRounds(): String = {
    // Initial round, only use roundKey
  }

  def aesAlgorithm(key: String): Unit = {
    // Create state (a 4x4 matrix of the bytes stored in the original key)

    // There are four steps
    // 1. KeyExpansions - Round keys are derived from cipher key using Rijndael's key scheduler
    // 2. 
  }
  
  def generateRijndaelSBox(i: Int, s: Int, result: Int): Array[Array[String]] = {
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
      val t = 0

      // 2. We assign the value of the previous four bytes in the expanded key to t
      val t2 = 0 + n.slice(n.length-4, n.length)

      // 3. We perform the key schedule core (see above) on t, with i as the rcon iteration value
    }
  }
  
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
}