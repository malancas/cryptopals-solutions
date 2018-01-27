package lib1

import collection.mutable.Stack
import org.scalatest._
import lib1.AES128

class AES128Test extends FunSpec with Matchers {
  describe("shiftRows") {
    val cipherState = Array.range(0,16)
    val correctShiftedState = Array(0,1,2,3,5,6,7,4,10,11,8,9,13,14,15,12)

    when(shiftRows(cipherState)).thenReturn(correctShiftedState)
  }
}

