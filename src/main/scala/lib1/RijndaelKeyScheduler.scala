package lib1.RijndaelKeyScheduler

class RijndaelKeyScheduler(rijndaelSBox: Array[Int]) {
  // Initialized rcon
  val rcon = Array(0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36)
  
  def makeRoundKeys(rconIteration: Int, currKey: Array[Int], rijndaelSBox: Array[Int]): Array[Array[Int]] = {
    if (currKey.length < 176) {
      // Create the next four bytes of the expanded key
      val temp = currKey.takeRight(4)
      val currExpandedKey = keySchedulerCore(rconIteration, temp)

      // Do the following three times to create the next twelve bytes of expanded key
      val newBytes = produce12BytesOfExpandedKey(0, currExpandedKey)
      makeRoundKeys(rconIteration+1, currKey ++ newBytes, rijndaelSBox)
    }
    // Create keys and return them
    else currKey.grouped(16).toArray
  }

  def keySchedulerCore(i: Int, input: Array[Int]): Array[Int] = {
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
    else currKey
  }
}