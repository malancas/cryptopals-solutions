/*
The hex encoded string:
1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
has been XOR'd against a single character. Find the key, decrypt the message.
You can do this by hand. But don't: write code to do it for you.
How? Devise some method for "scoring" a piece of English plaintext. Character
 frequency is a good metric. Evaluate each output and choose the one with the best score.
*/

// Step 1: Convert hex string to decimal
// Step 2: xor it with each possible character
// Step 2.1: Convert it back to hex
// Step 2.2: Get the ascii values (should result in plain text)
// Step 2.3: Score the plain text
// Step 3: compare the scores of each plain text and choose the best scoring one
import s1q1._
import s1q2._

def scorePlainText(plainText: String): Decimal

def convertHexToPlainText(hexStr: String): String = {
  
}

def decodeHexString(hexStr: String): String = {
  // Conver the hex string to decimalDigit
  var decimalNum = convertHexStringtoDecimalNum(0, hexStr, 0)

  // XOR it with each possible character from ascii table
  for (i <- 32 to 126) {
    // Get the ascii character that alligns with the given integer
    var charac = i.toChar
    var xored = decimalNum ^ i

    var xored-plaintext = convertHexToPlainText(convertDecimalToHex())
    var score = scorePlainText()
  }
}
