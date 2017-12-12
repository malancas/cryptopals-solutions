package lib1.BlockCipherMode

import lib1.AES128Matrix

trait BlockCipherMode {
  def divideTextIntoBlocks(text: String): List[List[Byte]] = {
    // Assume block size is 16 bytes for now
    text.grouped(4).toList.transpose.map(_.map(_.toByte))
  }
}

class ECBMode extends BlockCipherMode {
  def doECB(plaintext: String, key: String): String = {
    val textMatrix = new AES128Matrix(plaintext)
    val keyMatrix = new AES128Matrix(key)

    val textBlocks = divideTextIntoBlocks(plaintext)

    // Now use the initial cipher block and encrypt the remaining text blocks
    val encryptedText = processTextBlocks(textBlocks.tail, initialCipherBlock, key)
    encryptedText
  }

  def encryptBlock(textBlock: List[Byte], cipherBlock: List[Byte]): List[Byte] = {
    // XOR the text block with the previous ciphertext block
    val xored = textBlock.zip(cipherBlock).map { case (x,y) =>  x ^ y }

    // Encrypted the result
    val keyArray = key.getBytes()
    val encrypted = xored.zip(keyArray).map { case (x,y) => (x ^ y)}
    encrypted
  }

  override def divideTextIntoBlocks(text: String): Array[List[Byte]] = {
    // Assume block size is 16 bytes for now
    text.grouped(4).toList.transpose.toArray.map(_.map(_.toByte))
  }

  def processTextBlocks(textBlocks: List[List[Byte]], cipherBlock: List[Byte], encryptedText: String): String = {
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
}

class CBCMode(iv: String, keyInput: String) extends BlockCipherMode {
  val IV: List[Byte] = iv.toList.map(_.toByte)
  val key: String = keyInput

  def encryptBlock(textBlock: List[Byte], cipherBlock: List[Byte]): List[Byte] = {
    // XOR the text block with the previous ciphertext block
    val xored = textBlock.zip(cipherBlock).map { case (x,y) =>  x ^ y }

    // Encrypted the result
    val keyArray = key.getBytes()
    val encrypted = xored.zip(keyArray).map { case (x,y) => (x ^ y)} .map(_.toByte)
    encrypted
  }

  def processTextBlocks(textBlocks: List[List[Byte]], cipherBlock: List[Byte], encryptedText: String): String = {
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

  def doCBC(plaintext: String): String = {
    val textBlocks = divideTextIntoBlocks(plaintext)
    // Begin with encrypting the first textblock with the IV
    val initialCipherBlock = encryptBlock(textBlocks.head, IV)

    // Now use the initial cipher block and encrypt the remaining text blocks
    val encryptedText = processTextBlocks(textBlocks.tail, initialCipherBlock, key)
    encryptedText
  }
}