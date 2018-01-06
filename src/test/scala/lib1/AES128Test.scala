package lib1

import collection.mutable.Stack
import org.scalatest._
import lib1.AES128

class AES128Test extends FunSpec with Matchers {
  describe("AES128") {
    val aes128 = new AES128Impl

    it("should convert ints to their sbox result") {
      aes128.substituteWithHexToRijndaelBoxReplacement(0, 0x11, 0) shouldBe 130
    }
  }
}

