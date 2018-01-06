package lib1

class AES128Matrix(text: String) {
  // Convert the text in a 4X4 2D array representation
  val textMatrix = text.grouped(4).toList.transpose.toArray

  //def getColumn(columnNum: Int): List[Char] = {
  //  List(textMatrix.map(_(map(_columnNum))))
  //}
}