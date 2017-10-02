package c1

object C1 {
  def splitStringIntoArray(str: String, n: Int): Array[String] = {
    // This regex will split after every nth character
    // in the string. The number of dots signify
    // how many characters are skipped before splitting
    val splitRegex = "(?<=\\G" + "." * n + ")"
    str.split(splitRegex)
  }

  def convertDecimalDigitToBase64Digit(digit: Int): Char = {
    // Return the base 64 equivalent of the decimal digit argument
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
    else { // digit == 63
      '/'
    }
  }

  def convertHexStringToBinaryString(hexStr: String): String = {
    // Use BigInt to handle the long string. Need to add a 0 to the beginning
    // of the string for proper conversion. Otherwise, the lenght of the string
    // will lead to errors when decoding
    "0" + BigInt(hexStr, 16).toString(2)
  }

  def convertBinaryArrayToBase64Array(binaryArray: Array[String]): Array[Char] = {
    // Convert each element of binaryArray to its decimal equivalent before each
    // is converted to its base 64 equivalent
    binaryArray.map(Integer.parseInt(_, 2)).map(convertDecimalDigitToBase64Digit(_))
  }

  def convertHexStringtoBase64String(hexStr: String): String = {
    // Convert the hex string to a binary string
    val binaryStr = convertHexStringToBinaryString(hexStr)

    // Convert the binary string into an array of binary numbers by splitting on every sixth digit
    // Split is done after every sixth digit instead of eigth since base 64 numbers are only made
    // up of six binary values instead of eight like binary or decimal numbers
    //val binaryArray = binaryStr.split("(?<=\\G......)")
    val binaryArray = splitStringIntoArray(binaryStr, 6)

    // Convert each binary number element of the binary array into its base 64 equivalent
    val base64Array = convertBinaryArrayToBase64Array(binaryArray)

    // Make a string from the base 64 array
    base64Array.mkString("")
  }

  def solution(hexStr: String): String = {
    convertHexStringtoBase64String(hexStr)
  }
}
