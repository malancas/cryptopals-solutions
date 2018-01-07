package lib1

trait AES128 {}

class AES128Impl extends AES128 {
  // Initialized s box
  val rijndaelSBox = Map(
    0x00 -> Map(0x00 -> 0x63, 0x01 -> 0x7c, 0x02 -> 0x77, 0x03 -> 0x7b, 0x04 -> 0xf2, 0x05 -> 0x6b,
      0x06 -> 0x6f, 0x07 -> 0xc5, 0x08 -> 0x30, 0x09 -> 0x01, 0x0a -> 0x67, 0x0b -> 0x2b, 0x0c -> 0xfe,
      0x0d -> 0xd7, 0x0e -> 0xab, 0x0f -> 0x76), 
    0x10 -> Map(0x00 -> 0xca, 0x01 -> 0x82, 0x02 -> 0xc9, 0x03 -> 0x7d, 0x04 -> 0xfa, 0x05 -> 0x59,
      0x06 -> 0x47, 0x07 -> 0xf0, 0x08 -> 0xad, 0x09 -> 0xd4, 0x0a -> 0xa2, 0x0b -> 0xaf, 0x0c -> 0x9c,
      0x0d -> 0xa4, 0x0e -> 0x72, 0x0f -> 0xc0),
    0x20 -> Map(0x00 -> 0xb7, 0x01 -> 0xfd, 0x02 -> 0x93, 0x03 -> 0x26, 0x04 -> 0x36, 0x05 -> 0x3f,
      0x06 -> 0xf7, 0x07 -> 0xcc, 0x08 -> 0x34, 0x09 -> 0xa5, 0x0a -> 0xe5, 0x0b -> 0xf1, 0x0c -> 0x71,
      0x0d -> 0xd8, 0x0e -> 0x31, 0x0f -> 0x15),
    0x30 -> Map(0x00 -> 0x04, 0x01 -> 0xc7, 0x02 -> 0x23, 0x03 -> 0xc3, 0x04 -> 0x18, 0x05 -> 0x96,
      0x06 -> 0x05, 0x07 -> 0x9a, 0x08 -> 0x07, 0x09 -> 0x12, 0x0a -> 0x80, 0x0b -> 0xe2, 0x0c -> 0xeb,
      0x0d -> 0x27, 0x0e -> 0xb2, 0x0f -> 0x75),
    0x40 -> Map(0x00 -> 0x09, 0x01 -> 0x83, 0x02 -> 0x2c, 0x03 -> 0x1a, 0x04 -> 0x1b, 0x05 -> 0x6e,
      0x06 -> 0x5a, 0x07 -> 0xa0, 0x08 -> 0x52, 0x09 -> 0x3b, 0x0a -> 0xd6, 0x0b -> 0xb3, 0x0c -> 0x29,
      0x0d -> 0xe3, 0x0e -> 0x2f, 0x0f -> 0x84),
    0x50 -> Map(0x00 -> 0x53, 0x01 -> 0xd1, 0x02 -> 0x00, 0x03 -> 0xed, 0x04 -> 0x20, 0x05 -> 0xfc,
      0x06 -> 0xb1, 0x07 -> 0x5b, 0x08 -> 0x6a, 0x09 -> 0xcb, 0x0a -> 0xbe, 0x0b -> 0x39, 0x0c -> 0x4a,
      0x0d -> 0x4c, 0x0e -> 0x58, 0x0f -> 0xcf),
    0x60 -> Map(0x00 -> 0xd0, 0x01 -> 0xef, 0x02 -> 0xaa, 0x03 -> 0xfb, 0x04 -> 0x43, 0x05 -> 0x4d,
      0x06 -> 0x33, 0x07 -> 0x85, 0x08 -> 0x45, 0x09 -> 0xf9, 0x0a -> 0x02, 0x0b -> 0x7f, 0x0c -> 0x50,
      0x0d -> 0x3c, 0x0e -> 0x9f, 0x0f -> 0xa8),
    0x70 -> Map(0x00 -> 0x51, 0x01 -> 0xa3, 0x02 -> 0x40, 0x03 -> 0x8f, 0x04 -> 0x92, 0x05 -> 0x9d,
      0x06 -> 0x38, 0x07 -> 0xf5, 0x08 -> 0xbc, 0x09 -> 0xb6, 0x0a -> 0xda, 0x0b -> 0x21, 0x0c -> 0x10,
      0x0d -> 0xff, 0x0e -> 0xf3, 0x0f -> 0xd2),
    0x80 -> Map(0x00 -> 0xcd, 0x01 -> 0x0c, 0x02 -> 0x13, 0x03 -> 0xec, 0x04 -> 0x5f, 0x05 -> 0x97,
      0x06 -> 0x44, 0x07 -> 0x17, 0x08 -> 0xc4, 0x09 -> 0xa7, 0x0a -> 0x7e, 0x0b -> 0x3d, 0x0c -> 0x64,
      0x0d -> 0x5d, 0x0e -> 0x19, 0x0f -> 0x73),
    0x90 -> Map(0x00 -> 0x60, 0x01 -> 0x81, 0x02 -> 0x4f, 0x03 -> 0xdc, 0x04 -> 0x22, 0x05 -> 0x2a,
      0x06 -> 0x90, 0x07 -> 0x88, 0x08 -> 0x46, 0x09 -> 0xee, 0x0a -> 0xb8, 0x0b -> 0x14, 0x0c -> 0xde,
      0x0d -> 0x5e, 0x0e -> 0x0b, 0x0f -> 0xdb),
    0xa0 -> Map(0x00 -> 0xe0, 0x01 -> 0x32, 0x02 -> 0x3a, 0x03 -> 0x0a, 0x04 -> 0x49, 0x05 -> 0x06,
      0x06 -> 0x24, 0x07 -> 0x5c, 0x08 -> 0xc2, 0x09 -> 0xd3, 0x0a -> 0xac, 0x0b -> 0x62, 0x0c -> 0x91,
      0x0d -> 0x95, 0x0e -> 0xe4, 0x0f -> 0x79),
    0xb0 -> Map(0x00 -> 0xe7, 0x01 -> 0xc8, 0x02 -> 0x37, 0x03 -> 0x6d, 0x04 -> 0x8d, 0x05 -> 0xd5,
      0x06 -> 0x4e, 0x07 -> 0xa9, 0x08 -> 0x6c, 0x09 -> 0x56, 0x0a -> 0xf4, 0x0b -> 0xea, 0x0c -> 0x65,
      0x0d -> 0x7a, 0x0e -> 0xae, 0x0f -> 0x08),
    0xc0 -> Map(0x00 -> 0xba, 0x01 -> 0x78, 0x02 -> 0x25, 0x03 -> 0x2e, 0x04 -> 0x1c, 0x05 -> 0xa6,
      0x06 -> 0xb4, 0x07 -> 0xc6, 0x08 -> 0xe8, 0x09 -> 0xdd, 0x0a -> 0x74, 0x0b -> 0x1f, 0x0c -> 0x4b,
      0x0d -> 0x4b, 0x0e -> 0x8b, 0x0f -> 0x8a),
    0xd0 -> Map(0x00 -> 0x70, 0x01 -> 0x3e, 0x02 -> 0xb5, 0x03 -> 0x66, 0x04 -> 0x48, 0x05 -> 0x03,
      0x06 -> 0xf6, 0x07 -> 0x0e, 0x08 -> 0x61, 0x09 -> 0x35, 0x0a -> 0x57, 0x0b -> 0xb9, 0x0c -> 0x86,
      0x0d -> 0xc1, 0x0e -> 0x1d, 0x0f -> 0x93),
    0xe0 -> Map(0x00 -> 0xe1, 0x01 -> 0xf8, 0x02 -> 0x98, 0x03 -> 0x11, 0x04 -> 0x69, 0x05 -> 0xd9,
      0x06 -> 0x8e, 0x07 -> 0x94, 0x08 -> 0x9b, 0x09 -> 0x1e, 0x0a -> 0x87, 0x0b -> 0xe9, 0x0c -> 0xce,
      0x0d -> 0x55, 0x0e -> 0x28, 0x0f -> 0xdf),
    0xf0 -> Map(0x00 -> 0x8c, 0x01 -> 0xa1, 0x02 -> 0x89, 0x03 -> 0x0d, 0x04 -> 0xbf, 0x05 -> 0xe6,
      0x06 -> 0x42, 0x07 -> 0x68, 0x08 -> 0x41, 0x09 -> 0x99, 0x0a -> 0x2d, 0x0b -> 0x0f, 0x0c -> 0xb0,
      0x0d -> 0x54, 0x0e -> 0xbb, 0x0f -> 0x16)
  )

  def substitute1(input: Int): Int = {
    val inverseInputBits = Integer.toBinaryString(input).toArray.map {x => if (x == '1') '0' else '1'}.mkString("")
    println("reversed input bit string: " + inverseInputBits)

    val inverseNum = Integer.parseInt(inverseInputBits,2)
  
    //substitute1_aux(0, inverseNum, 0)
    //substitute1_aux(0, inverseNum, inverseNum)
    substitute1_aux(0, input, 0)  
  }

  def substitute1_aux(i: Int, input: Int, result: Int): Int = {
    /*
    1. Let s (an 8-bit unsigned variable) be the input number.
    2. Let result be 0.
    3. For 5 times:
        1. XOR result with s.
        2. Rotate s one bit to the left.

    */
    if (i < 5) {
      // XOR result with s
      //val newResult = input ^ result
      val newInput = input ^ result

      //val newInput = (((input & 0xff) << 1) | (input & 0xff) >>> (8 - 1))
      val newResult = (((input & 0xff) << 1) | (input & 0xff) >>> (8 - 1))      
      substitute1_aux(i+1, newInput, newResult) 
    }
    else {
      // Return result
      result ^ 99
    }
  }

  def substitute2(input: Int): Int = {
    // Store the multiplicative inverse of the input number in two 8-bit unsigned temporary variables: s and x.
    println("input bit string: " + Integer.toBinaryString(input))
    
    val inverseInputBits = Integer.toBinaryString(input).toArray.map {x => if (x == '1') '0' else '1'}.mkString("")
    println("reversed input bit string: " + inverseInputBits)

    val inverseNum = Integer.parseInt(inverseInputBits,2)
    val s = inverseNum
    val x = inverseNum

    val newX = substitute2_aux(0, x, s)
    newX ^ 99
  }

  def updateLowBit(s: Int): Int = {
    // If the value of s had a high bit (eighth bit from the right) of one,
    // make the low bit of s one; otherwise the low bit of s is zero.
    val sBits = Integer.toBinaryString(s).toArray
    println("sbits: " + sBits.mkString(""))
    val length = sBits.length
    if (length < 8 || sBits(0) == '0'){
      Integer.parseInt((sBits.slice(0,length-1) ++ Array('0')).mkString(""), 2)
    }
    else {
      Integer.parseInt((sBits.slice(0,length-1) ++ Array('1')).mkString(""), 2)      
    }
  }

  def substitute2_aux(i: Int, x: Int, s: Int): Int = {
    if (i == 4){
      x
    }
    else {
      println("s: " + s)
      val rotatedS = (((s & 0xff) << 1) | (s & 0xff) >>> (8 - 1))
      println("s: " + rotatedS)

      val updatedSBits = updateLowBit(rotatedS)
      println("updated s bits: " + Integer.toBinaryString(updatedSBits))

      // Exclusive or the value of x with the value of s, storing the value in x
      val xoredX = x ^ updatedSBits

      substitute2_aux(i+1, xoredX, updatedSBits)
    }
  }
}