package lib1

trait AES128 {}

class AES128Impl(key: String, ciphertext: String) extends AES128 {
   // Initialized s box
  val rijndaelSBox = Array(
    0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76,
    0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0,
    0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15,
    0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75,
    0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84,
    0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF,
    0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8,
    0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2,
    0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73,
    0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB,
    0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79,
    0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08,
    0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A,
    0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E,
    0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF,
    0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16
  )
  // Initialized rcon
  val rcon = Array(0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36)

  def doAlgorithm: String = {
    val keyArray = key.split("").map(_.toInt)
    val cipherState = ciphertext.split("").map(_.toInt)
    // Step 1: KeyExpansions - create round keys from the cipher key using the Rijndael Key Scheduler
    // Each round requires a 128 bit round key
    val roundKey = createRoundKey(keyArray)

    // Step 2: Initial round - each byte is XORed with a block of the round key
    val newCipherState = addRoundKey(cipherState, roundKey)

    // Do 9 main rounds
    doMainRounds(0, cipherState)

    "filler"
  }

  def doMainRounds(i: Int, cipherState: Array[Int]): Array[Int] = {
    // Substitute the ciphertext values for their s-box equivalent
    val subbedCipher = subBytes(cipherState)

    // Shift each row of the ciphertext
    val shiftedCipher = shiftRows(subbedCipher)

    // Mix columns
    val mixedCipher = mixColumns(shiftedCipher)
    Array(1)
  }

  def createRoundKey(keyArray: Array[Int]): Array[Int] = {
    rijndaelKeyScheduler(1, keyArray)
  }

  def keySchedulerCore(input: Array[Int], i: Int): Array[Int] = {
    // Rotate the input word eight bits to the left
    val rotated = input.slice(1, 16) ++ Array(input(0))

    // Apply the S-Box on all four elements of the word
    val sBoxWord = rotated.map(rijndaelSBox(_))

    // XOR the first byte of the word with the rcon output when using i
    val newFirstByte = rcon(i) ^ sBoxWord(0)

    Array(newFirstByte) ++ sBoxWord.slice(1, sBoxWord.length)
  }

  def produce12BytesOfExpandedKey(i: Int, currKey: Array[Int]): Array[Int] = {
    if (i < 3){
      // Append the last four bytes of the current extended key to temp
      val temp = currKey.takeRight(4)
      // XOR t with the four-byte block n bytes before the new expanded key.
      // This becomes the next 4 bytes in the expanded key
      val beginIt = currKey.length - 1 - 16 - 3
      val endIt = currKey.length - 1 - 16
      val nextFourBytes = temp.zip(currKey.slice(beginIt, endIt)).map{ case (x, y) => x ^ y }

      produce12BytesOfExpandedKey(i+1, currKey ++ nextFourBytes)
    }
    currKey
  }

  def rijndaelKeyScheduler(rconIteration: Int, currKey: Array[Int]): Array[Int] = {
    if (currKey.length < 176) {
      // Create the next four bytes of the expanded key
      val temp = currKey.takeRight(4)
      val currExpandedKey = keySchedulerCore(temp, rconIteration)

      // Do the following three times to create the next twelve bytes of expanded key
      val newBytes = produce12BytesOfExpandedKey(0, currExpandedKey)
      rijndaelKeyScheduler(rconIteration+1, currKey ++ newBytes)
    }
    else {
      currKey
    }
  }

  def addRoundKey(currCipherState: Array[Int], roundKey: Array[Int]): Array[Int] = {
    // Each element of the cipher state is XORed with the round key's equivalent element
    // These elements form the new cipher state
    currCipherState.zip(roundKey).map { case (x, y) => x ^ y}
  }

  def subBytes(currCipherState: Array[Int]): Array[Int] = {
    currCipherState.zipWithIndex.map { case (x, in) => rijndaelSBox(in) }
  }

  def shiftRow(row: Array[Int], shift: Int): Array[Int] = {
    // Split the array along the shift
    val (before, after) = row.splitAt(shift)
    after ++ before
  }

  def shiftRows(currCipherState: Array[Int]): Array[Int] = {
    // Shift each nth row to the left n times.
    // Ex. the 0th row is shifted 0 times, the 1st row is shifted 1 time, etc.
    // Create array from row and process it
    currCipherState.slice(0,4) ++ shiftRow(currCipherState.slice(4, 8), 1) ++ shiftRow(currCipherState.slice(8, 12), 2) ++ shiftRow(currCipherState.slice(12, 16), 3)
  }

  def makeBArray(i: Int, cipherStateColumn: Array[Int], bArray: Array[Int]): Array[Int] = {
    if (i < cipherStateColumn.length){
      val newEl = (cipherStateColumn(i) << 1) ^ (0x1b & cipherStateColumn(i) >> 7)
      makeBArray(i+1, cipherStateColumn, bArray ++ Array(newEl))
    }
    else {
      bArray
    }
  }

  def mixColumn(cipherStateColumn: Array[Int]): Array[Int] = {
    val a = cipherStateColumn
    val b = makeBArray(0, cipherStateColumn, Array.empty)
    val new0 = b(0) ^ a(3) ^ a(2) ^ b(1) ^ a(1)
    val new1 = b(1) ^ a(0) ^ a(3) ^ b(2) ^ a(2)
    val new2 = b(2) ^ a(1) ^ a(0) ^ b(3) ^ a(3)
    val new3 = b(3) ^ a(2) ^ a(1) ^ b(0) ^ a(0)
    Array(new0, new1, new2, new3)
  }

  def mixColumns(currCipherState: Array[Int]): Array[Int] = {
    val column0 = Array(currCipherState(0), currCipherState(4), currCipherState(9), currCipherState(12))
    val column1 = Array(currCipherState(1), currCipherState(5), currCipherState(10), currCipherState(13))
    val column2 = Array(currCipherState(2), currCipherState(6), currCipherState(11), currCipherState(14))
    val column3 = Array(currCipherState(3), currCipherState(7), currCipherState(12), currCipherState(15))    
  
    val newCol0 = mixColumn(column0)
    val newCol1 = mixColumn(column1)
    val newCol2 = mixColumn(column2)
    val newCol3 = mixColumn(column3)

    val row0 = Array(newCol0(0), newCol1(0), newCol2(0), newCol3(0))
    val row1 = Array(newCol0(1), newCol1(1), newCol2(1), newCol3(1))
    val row2 = Array(newCol0(2), newCol1(2), newCol2(2), newCol3(2))
    val row3 = Array(newCol0(3), newCol1(3), newCol2(3), newCol3(3))  
  
    row0 ++ row1 ++ row2 ++ row3
  }
}