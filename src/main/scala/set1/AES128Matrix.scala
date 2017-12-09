package set1

class AES128Matrix(text: String) {
  // Convert the text in a 4X4 2D array representation
  textMatrix = text.grouped(4).toList.transpose.toArray

  def getColumn(columnNum: Int): Array[String] = {
    Array(textMatrix.map(_(columnNum))
  }
}