package set2.c10

import scala.io.Source

/*
In CBC mode, each block of plaintext is XORed with the previous ciphertext block before being encrypted. 
This way, each ciphertext block depends on all plaintext blocks processed up to that point. 
To make each message unique, an initialization vector must be used in the first block.
*/

class CBCMode(iv: Array[Int], key: String) {
  val IV: Array[Int] = iv
  val Key: String = key

  def encryptBlock(textBlock: Array[Byte], cipherBlock: Array[Int]): Array[Int] = {
    // XOR the text block with the previous ciphertext block
    val xored = textBlock.zip(cipherBlock).map { case (x,y) =>  x ^ y }

    // Encrypted the result
    val keyArray = key.getBytes()
    val encrypted = xored.zip(keyArray).map { case (x,y) => (x ^ y)}
    encrypted
  }

  def divideTextIntoBlocks(text: String): List[Array[Byte]] = {
    // Assume block size is 16 bytes for now
    text.getBytes.grouped(16).map(_.toArray).toList
  }

  def processTextBlocks(textBlocks: List[Array[Byte]], cipherBlock: Array[Int], encryptedText: String): String = {
    textBlocks match {
      case h :: t => {
        // Process the first block
        val encryptedBlock = encryptBlock(h, cipherBlock)
        val encryptedText = encryptedBlock.map(_.toChar).mkString("")
        processTextBlocks(t, encryptedBlock, encryptedText + encryptedBlock)
      }
      case Nil => {
        encryptedText
      }
    }
  }

  def CBCMode(plaintext: String, key: String): String = {
    val textBlocks = divideTextIntoBlocks(plaintext)
    // Begin with encrypting the first textblock with the IV
    val initialCipherBlock = encryptBlock(textBlocks.head, IV)

    // Now use the initial cipher block and encrypt the remaining text blocks
    val encryptedText = processTextBlocks(textBlocks.tail, initialCipherBlock, key)
    encryptedText
  }

  def solution(): Unit = {
    val encryptedText = Source.fromResource("10.txt").getLines.mkString("")
  }
}