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

  describe("shiftRow") {
    it ("should shift the row by one element") {
      val row = Array.range(0,4)
      val shiftedRow = Array(1,2,3,0)
      aes128Impl.shiftRow(row, 1) shouldBe shiftedRow
    }

    it ("should shift the row by two elements") {
      val row = Array.range(0,4)
      val shiftedRow = Array(2,3,0,1)
      aes128Impl.shiftRow(row, 2) shouldBe shiftedRow
    }

    it ("should shift the row by three elements") {
      val row = Array.range(0,4)
      val shiftedRow = Array(3,0,1,2)
      aes128Impl.shiftRow(row, 3) shouldBe shiftedRow
    }

    it ("should shift the row by four elements") {
      val row = Array.range(0,4)
      aes128Impl.shiftRow(row, 4) shouldBe row
    }

    it ("should shift the row by zero elements") {
      val row = Array.range(0,4)
      aes128Impl.shiftRow(row, 0) shouldBe row
    }
  }

  describe("shiftRows") {
    it ("should return shifted 0-16 array") {
      val rows = Array.range(0,16)
      val shiftedRows = Array(0,1,2,3,5,6,7,4,10,11,8,9,15,12,13,14)
      aes128Impl.shiftRows(rows) shouldBe shiftedRows
    }
  }

  describe("subBytes") {
    it ("should return s-box equivalents of cipher state array") {
      val cipherState = Array.range(0,16)
      val subbedState = Array(0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76)
      aes128Impl.subBytes(cipherState) shouldBe subbedState
    }
  }
}