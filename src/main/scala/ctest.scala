package ctest

import java.io._
import c1.C1
import c5.C5
import c6.C6

class CTest {
    def decryptWithAllKeys(binaryCiphertext: String, keys: Array[(Double, Int)]): Unit = {
        val c6 = new C6

        // FileWriter
        val file = new File("testResults.txt")
        val bw = new BufferedWriter(new FileWriter(file))
        
        for (i <- 0 until keys.length){

            // Decrypt using only one key size
            val repeatingXORKey = c6.getRepeatingKeyXORWithChosenKeySize(keys(i)._2, binaryCiphertext)
            //println(s"repeating key: $repeatingXORKey")

            // Convert binary ciphertext to its decimal equivalent
            val c = new C1
            val characterCiphertext = c
                .splitStringIntoArray(binaryCiphertext, 8)
                .map(Integer.parseInt(_, 2).toChar)
                .mkString("")

            //println(s"ciphertext: $characterCiphertext")

            val charKey = c
                .splitStringIntoArray(repeatingXORKey, 1)

            // Decode the file contents with the key
            val c5 = new C5
            val decryptedText = c5.encodeStringWithRepeatingKeyXOR(characterCiphertext, repeatingXORKey)
            val decryptedText2 = c.splitStringIntoArray(decryptedText, 2).map(Integer.parseInt(_, 16).toChar).mkString("")

            val keySize = keys(i)._2
            val str = "keySize: " + keySize
            bw.write(str)  
            bw.write(decryptedText2)
            bw.write('\n')
        }
        bw.close()
    }

    def decryptTest(): Unit = {
        val c6 = new C6
        
        // Form a list of the file contents
        val fileLines = c6.getLinesFromFile()

        // Form a single string made up of the file contents
        val base64CiphertextList = fileLines.mkString("").toList

        // Convert the base 64 encoded text from the file to a binary string
        val binaryCiphertext = base64CiphertextList.map(c6.convertBase64DigitToSixDigitBinaryString(_)).mkString("")

        val keySizes = (2 to 50).toArray.map(x => (0.0, x))

        decryptWithAllKeys(binaryCiphertext, keySizes)
    }
}