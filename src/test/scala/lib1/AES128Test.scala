package lib1

import collection.mutable.Stack
import org.scalatest._
import lib1.AES128

class AES128Test extends FunSpec with Matchers {
  describe("AES128") {
    val aes128 = new AES128Impl

    it("sub1 should convert ints to their sbox result") {
      aes128.substitute1(0) shouldBe 0x63 // 124
    }    

    it("sub2 should convert ints to their sbox result") {
      aes128.substitute2(0) shouldBe 0x63 // 124
    }
  }
}

