/*
The hex encoded string:
1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
has been XOR'd against a single character. Find the key, decrypt the message.
You can do this by hand. But don't: write code to do it for you.
How? Devise some method for "scoring" a piece of English plaintext. Character
 frequency is a good metric. Evaluate each output and choose the one with the best score.

 Step 1: Convert hex string to decimal
 Step 1.1: Break up string into Array of hex characters
 Step 1.2: Convert each hex character in decimal and xor it with a potential key
 Step 1.3: Convert each character to its ascii character equivalent
 Step 2: Score the plain text array (letter frequency)
 Step 3: compare the scores of each plain text and choose the best scoring one
*/
package c13

class C13 {
  def scorePlainText(plainText: Array[String]): Decimal

  def convertDecimalDigitToAsciiCharacter(digit: Int): Char = {
    // Return the ascii character equivalent of each integer
    digit.asChar
  }

  def convertHexStringToPlainTextArray(hexStr: String, key: Int): Array[String] = {
    hexStr
      .split("(?<=\\G..)")
      .map(Integer.parseInt(_, 16))
      .map(_ ^ key)
      .map(convertDecimalDigitToAsciiCharacter(_))
  }

  def decodeHexString(hexStr: String): String = {
    // Conver the hex string to decimalDigit
    val decimalNum = convertHexStringtoDecimalNum(0, hexStr, 0)

    // XOR it with each possible character from ascii table
    for (i <- 32 to 126) {
      val plainText = convertHexStringToPlainTextArray(hexStr, i)
      val score = scorePlainText(plainText)
    }
  }
}
