def convertHexDigitToDecimal(hexDigit: Char, power: Int): Int = {
  hexDigit.asDigit * 16 * power
}

def convertHexStringtoDecimalNum(i: Int, hexStr: String, currSum: Int): Int = {
  // The power is computed by
  var power = hexStr.lenght - i - 1
  var decimalDigit = convertHexDigitToDecimal(hexStr(i), power)

  if (i == 0){
    currSum + decimalDigit
  }
  else {
    convertHexStringtoDecimalNum(i+1, hexStr, currSum + decimalDigit)
  }
}

def convertDecimalDigitToBase64Digit(digit: Int): Char = {
  // Return the ascii character equivalent of each integer
  digit match {
    case (0 <= digit && digit <= 15) => (digit + 65).asChar
    case (16 <= digit && digit <= 51) => (digit + 81).asChar
    case (_) => (digit - 4).asChar
  }
}

def convertDecimalToBase64(i: Int, decimalNum: Int, base64Str: String): String = {
  // The remainder is converted into its base 64 equivalent and used to build
  // the base 64 string
  var remainder = decimalNum % 64
  var dividend  = decimalNum / 64
  var base64Digit = convertDecimalDigitToBase64Digit(remainder)

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
  var decimalNum = convertHexStringtoDecimal(hexStr.length-1, hexStr, 0)

  // Then convert the decimal string to a base 64 string
  convertDecimalToBase64(0, decimalNum, "")
}
