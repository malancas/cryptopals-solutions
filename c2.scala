// Write a function that takes two equal-length
// buffers and produces the hex representation of their XOR combination
package c2

class C2 {
  def convertHexArrayToDecimalArray(hexArr: Array[String]): Array[Int] = {
    hexArr.map(Integer.parseInt(_, 16))
  }

  def getFixedXORofHexBuffers(buffer0: String, buffer1: String): String = {
    val decimalBuffer0 = convertHexArrayToDecimalArray(buffer0.split("(?<=\\G..)"))
    val decimalBuffer1 = convertHexArrayToDecimalArray(buffer1.split("(?<=\\G..)"))

    val xoredDecimalArray = decimalBuffer0.zip(decimalBuffer1).map { case (x, y) => x ^ y }

    val hexArray = xoredDecimalArray.map("%x".format(_))

    hexArray.mkString("")
  }
}
