package lib1

import collection.mutable.Stack
import org.scalatest._
import lib1.AES128

class AES128Test extends FunSpec with Matchers {
  describe("AES128") {
    val aes128 = new AES128Impl

    /*
    it("sub1 should convert ints to their sbox result") {
      //aes128.substitute1(0) shouldBe 0x63 // 130
      //aes128.substitute1(17) shouldBe 0x82 // 124
      aes128.substitute1(0x9a) shouldBe 0xb8 // 184     
    }   
    */
    
    it("sub2 should convert ints to their sbox result") {
      aes128.substitute3(0x9a) shouldBe 0xb8
    }
  }
}

