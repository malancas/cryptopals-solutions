package set2.c10

import scala.io.Source
import lib1.BlockCipherMode // Something wrong with this import

/*
In CBC mode, each block of plaintext is XORed with the previous ciphertext block before being encrypted. 
This way, each ciphertext block depends on all plaintext blocks processed up to that point. 
To make each message unique, an initialization vector must be used in the first block.
*/

object C10 {
  def solution: String = {
    val encryptedText = Source.fromResource("10.txt").getLines.mkString("")
    val cbcMode = new BlockCipherMode.CBCMode("0000", "YELLOW SUBMARINE")
    cbcMode.doCBC(encryptedText)
  }
}