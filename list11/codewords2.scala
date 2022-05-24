object codewords2 {

  import scala.math.{floorMod, pow}

  def hammingWeight(v: Int): Int = {
    var cnt = 0
    var tmp = v
    while (tmp != 0) {
      tmp = tmp & (tmp - 1)
      cnt = cnt + 1
    }
    cnt
  }
  def hammingDistance(a: Int, b: Int): Int = {
    hammingWeight(a ^ b)
  }

  def toBinary(i: Int, digits: Int) = {
    val res =
      String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
    if (res.length > digits) {
      val cut = res.length - digits
      res.dropRight(cut)
    } else res
  }

  def isValidCodeword(x: Int): Boolean = {
    val p1Check = hammingWeight( x & 0x68 ) % 2 == ((x >>> 2) & 1) // 0x68 = 0b1101000 - extracting d1, d2 and d4
    val p2Check = hammingWeight( x & 0x58 ) % 2 == ((x >>> 1) & 1) // 0x58 = 0b1011000 - extracting d1, d3 and d4
    val p3Check = hammingWeight( x & 0x38 ) % 2 == (x & 1)         // 0x38 = 0b0111000 - extracting d2, d3 and d4

    return p1Check & p2Check & p3Check
  }

  def appendParityBits(x: Int): Int = {
    assert(x < 16)
    val d1 = (x >>> 3) & 1
    val d2 = (x >>> 2) & 1
    val d3 = (x >>> 1) & 1
    val d4 = x & 1

    val p1 = (d1 + d2 + d4) % 2
    val p2 = (d1 + d3 + d4) % 2
    val p3 = (d2 + d3 + d4) % 2
    (x << 3) | ((p1 << 2) | (p2 << 1) | p3)
  }

  def main(args: Array[String]): Unit = {

    val codeword = "1011010"
    val x = Integer.parseInt(codeword, 2)
    if(isValidCodeword(x)) println(codeword + " is valid codeword.")
    val dist = 3

    val message = codeword.dropRight(3)
    val parityBits = codeword.takeRight(3)

    val codewords =
      (0 to pow(2, message.length).toInt - 1).map(x => appendParityBits(x))

    if (codewords.length == codewords.count(s => isValidCodeword(s))) println("Codewords generated correctly.")
    
    val close_codewords = codewords.filter(c => hammingDistance(c,x) == dist )

    println(
      "There are " + close_codewords.length.toString + " codewords of Hamming distance equal " + dist.toString
    )
    close_codewords.foreach(x => println(toBinary(x, 7)))
  
  }
}
/*
    Output:
      1011010 is valid codeword.
      Codewords generated correctly.
      There are 7 codewords of Hamming distance equal 3
      0010011
      0011100
      0101010
      1000110
      1001001
      1110000
      1111111
 */
