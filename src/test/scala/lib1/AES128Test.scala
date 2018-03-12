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
      val correctShiftedState = Array(0,1,2,3,5,6,7,4,10,11,8,9,15,12,13,14)
      aes128Impl.shiftRows(cipherState) shouldBe correctShiftedState
    }
  }

  describe("addRoundKey") {
    it ("should return a zero array") {
      val column = Array.range(0,16)
      val roundKey = Array.range(0,16)
      val zeroArray = Array.fill(16)(0)
      aes128Impl.addRoundKey(column, roundKey) shouldBe zeroArray
    }

    it ("should return the identity array") {
      val column = Array.range(0,16)
      val roundKey = Array.fill(16)(0)
      aes128Impl.addRoundKey(column, roundKey) shouldBe column
    }
  }

  describe("shiftRows") {
    it ("should return shifted 0-16 array") {
      val row = Array.range(0,16)
      val shiftedRow = Array(0,1,2,3,5,6,7,4,10,11,8,9,15,12,13,14)
      aes128Impl.shiftRows(row) shouldBe shiftedRow
    }
  }
}