package c11

class C11 {
  def convertHexDigitToDecimalForNum(hexDigit: String, power: Int): Int = {
    Integer.parseInt(hexDigit, 16) * 16 * power
  }

  def convertHexStringtoDecimalNum(i: Int, hexStr: String, currSum: Int): Int = {
    // The power and decimal digit are computed
    val power = hexStr.lenght - i - 1
    val decimalDigit = convertHexDigitToDecimalNum(hexStr.slice(i, i+2), power)

    // If i == 0, the string has been converted and be returned
    if (i == 0){
      currSum + decimalDigit
    }
    // Otherwise, recurse again
    else {
      convertHexStringtoDecimalNum(i - 2, hexStr, currSum + decimalDigit)
    }
  }

  def convertDecimalDigitToBase64Digit(digit: Int): Char = {
    // Return the ascii character equivalent of each integer
    if (0 <= digit && digit <= 15) {
      (digit + 65).asChar
    }
    else if (16 <= digit && digit <= 51) {
      (digit + 81).asChar
    }
    else {
      (digit - 4).asChar
    }
  }

  def convertDecimalToBase64(i: Int, decimalNum: Int, base64Str: String): String = {
    // The remainder is converted into its base 64 equivalent and used to build
    // the base 64 string
    val remainder = decimalNum % 64
    val dividend  = decimalNum / 64
    val base64Digit = convertDecimalDigitToBase64Digit(remainder)

    // If the dividend is zero, the final base 64 string can be returned
    if (dividend == 0){
      base64Digit + base64Str
    }

    // Otherwise, call the function again
    else {
      convertDecimalToBase64(i+1, dividend, base64Digit + base64Str)
    }
  }

  def convertHexStringtoBase64String(hexStr: String): String = {
    // Convert the hex string to a decimal number first
    val decimalNum = convertHexStringtoDecimalNum(hexStr.length-1, hexStr, 0)

    // Then convert the decimal string to a base 64 string
    convertDecimalToBase64(0, decimalNum, "")
  }
}
