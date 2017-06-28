// Write a function that takes two equal-length
// buffers and produces their XOR combination

// Step one: Decode both hex strings to binary
// Step two: XOR the decoded strings
// Step three: Encode the result to hex
import s1q1._

def convertDecimalDigitToHexDigit(decimalDigit: Int): String = {
  var charDigit = (decimalDigit + 48).toChar
  decimalDigit match {
    case (0 <= decimalDigit && decimalDigit <= 9) => "0" + charDigit
    case (_) => (decimalDigit + 87).toChar
  }
}

def convertDecimaltoHex(decimalNum: Int, currHexStr: String): String = {
  var remainder = decimalNum % 16
  var dividend = decimalNum / 16

  if (dividend == 0){
    remainder.toChar + currBinaryStr
  }
  else {
    convertDecimaltoHex(dividend, remainder.toString + currHexStr)
  }
}

def getFixedXORofHexBuffers(buffer0: String, buffer1: String): String = {
  // Decode hex to decimal
  var decimalNum0 = convertHexStringtoDecimalNum(0, buffer0, 0)
  var decimalNum1 = convertHexStringtoDecimalNum(0, buffer1, 0)

  // XOR the the decimal numbers
  var xored = decimalNum0 ^ decimalNum1

  // Convert the result to hex
  convertDecimaltoHex(xored, "")
}
