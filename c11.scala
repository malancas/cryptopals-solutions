package c11

class C11 {
  def convertDecimalDigitToBase64Digit(digit: Int): Char = {
    // Return the ascii character equivalent of each integer
    if (0 <= digit && digit <= 25) {
      (digit + 65).toChar
    }
    else if (26 <= digit && digit <= 51) {
      (digit + 71).toChar
    }
    else if (52 <= digit && digit <= 61) {
      (digit - 4).toChar
    }
    else if (digit == 62) {
      '+'
    }
    else {
      '/'
    }
  }

  def convertHexStringToBinaryString(hexStr: String): String = {
    // Use BigInt to handle the long string. Need to add a 0 to the beginning
    // of the string for proper conversion
    "0" + BigInt(hexStr, 16).toString(2)
  }

  def convertHexStringtoBase64String(hexStr: String): String = {
    // Convert the hex string to a binary string and add a 0 to the beginning
    val binaryStr = convertHexStringToBinaryString(hexStr)

    // Convert the binary string into an array of binary numbers by splitting on every sixth digit
    val binaryArray = binaryStr.split("(?<=\\G......)")

    // Convert each binary number element of the binary array into its base 64 equivalent
    val base64Array = binaryArray.map(Integer.parseInt(_, 2)).map(convertDecimalDigitToBase64Digit(_))

    // Convert the base 64 array into a string and return it
    base64Array.mkString("")
  }
}
