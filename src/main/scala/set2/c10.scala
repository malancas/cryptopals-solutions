package set2.c10

import javax.crypto.spec.SecretKeySpec
import javax.crypto.Cipher
import scala.io.Source
import java.util.Base64

/*
In CBC mode, each block of plaintext is XORed with the previous ciphertext block before being encrypted. 
This way, each ciphertext block depends on all plaintext blocks processed up to that point. 
To make each message unique, an initialization vector must be used in the first block.
*/

object C10 {
  // IV should be entirely made up of ASCII 0
  val IV: String = "\\x00\\x00\\x00 &c"

  def encryptBlock(textBlock: String, cipherBlock: String, key: String): String = {
    // XOR the text block with the previous ciphertext block
    val xored = textBlock ^ cipherBlock

    // Encrypted the result
    xored ^ key
  }

  def divideTextIntoBlocks(text: String): Array[String] = {
    // Assume block size is 16 bytes for now
    text.getBytes.grouped(16).map().toArray
  }

  def processTextBlocks(textBlocks: List[String], cipherBlock: String, encryptedText: String): String = {
    textBlocks match {
      case h :: t => {
        // Process the first block
        val encryptedBlock = encryptBlock(h, cipherBlock)
        processTextBlocks(t, encryptBlock, encryptedText ++ encryptedBlock)
      }
      case Nil => {
        encryptedText
      }
    }
  }

  def CBCMode(plaintext: String, key: String): String = {
    val textBlocks = divideTextIntoBlocks(plaintext)
    processTextBlocks(textBlocks, IV, key)
  }

  def solution(): String = {
    val encryptedText = Source.fromResource("10.txt").getLines.mkString("")
    val key = "YELLOW SUBMARINE"
  }
}