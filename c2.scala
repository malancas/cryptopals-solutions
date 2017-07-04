// Write a function that takes two equal-length
// buffers and produces the hex representation of their XOR combination
package c2

class C2 {
  def convertHexArrayToDecimalArray(hexArray: Array[String]): Array[Int] = {
    // Convert each hex element to its decimal equivalent
    hexArray.map(Integer.parseInt(_, 16))
  }

  def getFixedXORofHexBuffers(hexBuffer0: String, hexBuffer1: String): String = {
    // Convert each hex buffer to an array equivalent 
    val decimalBuffer0 = convertHexArrayToDecimalArray(hexBuffer0.split("(?<=\\G..)"))
    val decimalBuffer1 = convertHexArrayToDecimalArray(hexBuffer1.split("(?<=\\G..)"))

    val xoredDecimalArray = decimalBuffer0.zip(decimalBuffer1).map { case (x, y) => x ^ y }

    val hexArray = xoredDecimalArray.map("%x".format(_))

    hexArray.mkString("")
  }
}
