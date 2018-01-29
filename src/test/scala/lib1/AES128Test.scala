package lib1

import collection.mutable.Stack
import org.scalatest._
import lib1.AES128

import org.mockito.Mockito._
import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}
import org.scalatest.mock.MockitoSugar

class AES128Test extends FunSpec with Matchers with BeforeAndAfterEach with MockitoSugar {
  val aes128Impl = new AES128Impl("YELLOW SUBMARINE", "000102030405060708090a0b0c0d0e0f")
  
  describe("shiftRows") {
    it ("should return the correctShiftedState") {
      val cipherState = Array.range(0,16)
      val correctShiftedState = Array(0,1,2,3,5,6,7,4,10,11,8,9,13,14,15,12)
      aes128Impl.shiftRows(cipherState) shouldBe correctShiftedState
    }
  }
}

