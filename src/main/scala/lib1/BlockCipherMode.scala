package lib1

//import lib1.AES128Matrix

trait BlockCipherMode {
  def divideTextIntoBlocks(text: String): List[List[Int]] = {
    // Assume block size is 16 Ints for now
    text.grouped(4).toList.transpose.map(_.map(_.toInt))
  }
}

class ECBMode(keyInput: String) extends BlockCipherMode {
  val key = keyInput
  def doECB(plaintext: String, key: String): String = {
    val textMatrix = new AES128Matrix(plaintext)
    val keyMatrix = new AES128Matrix(key)
    val initialCipherBlock: List[Int] = List(0x01)


    val textBlocks: List[List[Int]] = divideTextIntoBlocks(plaintext)

    // Now use the initial cipher block and encrypt the remaining text blocks
    val encryptedText = processTextBlocks(textBlocks.tail, initialCipherBlock, key)
    encryptedText
  }

  def encryptBlock(textBlock: List[Int], cipherBlock: List[Int]): List[Int] = {
    // XOR the text block with the previous ciphertext block
    val xored = textBlock.zip(cipherBlock).map { case (x,y) =>  x ^ y }

    // Encrypted the result
    val keyArray = key.getBytes.map(_.toInt)
    val encrypted = xored.zip(keyArray).map { case (x,y) => (x ^ y)}
    encrypted
  }

  //override def divideTextIntoBlocks(text: String): Array[List[Int]] = {
    // Assume block size is 16 Ints for now
  //  text.grouped(4).toList.transpose.toArray.map(_.map(_.toInt))
  //}

  def processTextBlocks(textBlocks: List[List[Int]], cipherBlock: List[Int], encryptedText: String): String = {
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
  val IV: List[Int] = iv.toList.map(_.toInt)
  val key: String = keyInput

  def encryptBlock(textBlock: List[Int], cipherBlock: List[Int]): List[Int] = {
    // XOR the text block with the previous ciphertext block
    val xored = textBlock.zip(cipherBlock).map { case (x,y) =>  x ^ y }

    // Encrypted the result
    val keyArray = key.getBytes.map(_.toInt)
    val encrypted = xored.zip(keyArray).map { case (x,y) => (x ^ y)} .map(_.toInt)
    encrypted
  }

  def processTextBlocks(textBlocks: List[List[Int]], cipherBlock: List[Int], encryptedText: String): String = {
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