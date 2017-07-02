package c11

class C11 {
  /*
  def convertHexDigitToDecimalForNum(hexDigit: String, power: Int): Double = {
    println(s"Hexdigit: $hexDigit")
    val dec = Integer.parseInt(hexDigit, 16)
    println(s"hex to dec: $dec")
    println(s"pow: $power")
    Integer.parseInt(hexDigit, 16) * scala.math.pow(16, power)
  }

  def convertHexStringtoDecimalNum(i: Int, hexStr: String, currSum: Double): Double = {
    //println(s"hextr: $hexStr")
    //println(s"i: $i")
    // The power and decimal digit are computed
    val power = hexStr.length - i - 1
    val decimalDigit = convertHexDigitToDecimalForNum(hexStr.substring(i, i+1), power)
    println(s"decimalDigit: $decimalDigit")
    // If i == 0, the string has been converted and be returned
    if (i == 0){
      currSum + decimalDigit
    }
    // Otherwise, recurse again
    else {
      convertHexStringtoDecimalNum(i - 1, hexStr, currSum + decimalDigit)
    }
  }

  def convertDecimalDigitToBase64Digit(digit: Double): Char = {
    // Return the ascii character equivalent of each integer
    if (0 <= digit && digit <= 15) {
      (digit + 65).toChar
    }
    else if (16 <= digit && digit <= 51) {
      (digit + 81).toChar
    }
    else {
      (digit - 4).toChar
    }
  }

  def convertDecimalToBase64(i: Int, decimalNum: Double, base64Str: String): String = {
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
  */
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
    else if (digit == 63) {
      '/'
    }
    else {
      '?'
    }
  }

  def convertHexStringToDecimal(hexList: Array[String]): Array[Int] = {
    hexList.map(Integer.parseInt(_, 16))
  }

  def convertHexStringtoBase64String(hexStr: String): String = {
    // Convert the hex string to an array of individual hex characters
    val hexArray = hexStr.split("(?<=\\G..)")

    // Convert the hex array to a plain text array
    val plainTextArray = hexArray.map(Integer.parseInt(_, 16)).map(_.toChar)

    // Convert the hex array to a decimal array
    val decimalArray = hexArray.map(Integer.parseInt(_, 16))

    // Convert decimal array to a binary string
    val fullBinaryString = decimalArray.map(_.toBinaryString).mkString("")

    // Every six bits is a base 64 digit, so the binary string must be split
    // every six characters
    val binaryArray = fullBinaryString.split("(?<=\\G......)")

    // Convert the binary array to a base 64 digit array
    val base64Array = binaryArray.map(Integer.parseInt(_, 2)).map(convertDecimalDigitToBase64Digit(_))

    // Convert the base 64 array into a string and return it
    base64Array.mkString("")
  }
}
