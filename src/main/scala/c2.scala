// Write a function that takes two equal-length
// buffers and produces the hex representation of their XOR combination
package c2

import c1.C1

object C2 {
  def convertHexArrayToDecimalArray(hexArray: Array[String]): Array[Int] = {
    // Convert each hex element to its decimal equivalent
    hexArray.map(Integer.parseInt(_, 16))
  }

  def getFixedXORofHexBuffers(hexBuffer0: String, hexBuffer1: String): String = {
    // Convert each hex string to an array equivalent
    val hexArray0 = C1.splitStringIntoArray(hexBuffer0, 2)
    val hexArray1 = C1.splitStringIntoArray(hexBuffer1, 2)

    // Convert each element of the hex array to their decimal equivalents
    val decimalArray0 = convertHexArrayToDecimalArray(hexArray0)
    val decimalArray1 = convertHexArrayToDecimalArray(hexArray1)

    // XOR the elements of the decimal arrays together
    val xoredDecimalArray = decimalArray0.zip(decimalArray1).map { case (x, y) => x ^ y }

    // Convert each element of decimal array to their hex equivalents
    val hexArray = xoredDecimalArray.map("%x".format(_))

    // Make a string from the hex array
    hexArray.mkString("")
  }

  def solution(hexBuffer0: String, hexBuffer1: String): String = {
    getFixedXORofHexBuffers(hexBuffer0, hexBuffer1)
  }
}
