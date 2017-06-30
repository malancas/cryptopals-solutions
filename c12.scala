// Write a function that takes two equal-length
// buffers and produces their XOR combination

// Step one: Decode both hex strings to binary
// Step two: XOR the decoded strings
// Step three: Encode the result to hex
package c12

class C12 {
  def convertDecimalDigitToHexDigit(decimalDigit: Int): String = {
    val charDigit = (decimalDigit + 48).toChar

    if (0 <= decimalDigit && decimalDigit <= 9) {
      ("0" + charDigit)
    }
    else {
      (decimalDigit + 87).toChar
    }
  }

  def convertDecimaltoHex(decimalNum: Int, currHexStr: String): String = {
    val remainder = decimalNum % 16
    val dividend = decimalNum / 16

    if (dividend == 0){
      remainder.toChar + currBinaryStr
    }
    else {
      convertDecimaltoHex(dividend, remainder.toString + currHexStr)
    }
  }

  def getFixedXORofHexBuffers(buffer0: String, buffer1: String): String = {
    // Decode hex to decimal
    val decimalNum0 = convertHexStringtoDecimalNum(0, buffer0, 0)
    val decimalNum1 = convertHexStringtoDecimalNum(0, buffer1, 0)

    // XOR the the decimal numbers
    val xored = decimalNum0 ^ decimalNum1

    // Convert the result to hex
    convertDecimaltoHex(xored, "")
  }
}
