object codewords {

  import scala.math.floorMod

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

  def closeBitstrings(code: Int, distance: Int): List[Int] = {

    val subsets = (0 to code.toBinaryString.length - 1) combinations distance
    subsets
      .map(s => {
        var y = code
        s.foreach(k => y ^= (1 << k))
        y
      })
      .toList

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
    val p1Check =
      hammingWeight(
        x & 0x68
      ) % 2 == ((x >>> 2) & 1) // 0x68 = 0b1101000 - extracting d1, d2 and d4
    val p2Check =
      hammingWeight(
        x & 0x58
      ) % 2 == ((x >>> 1) & 1) // 0x58 = 0b1011000 - extracting d1, d3 and d4
    val p3Check =
      hammingWeight(
        x & 0x38
      ) % 2 == (x & 1) // 0x38 = 0b0111000 - extracting d2, d3 and d4

    return p1Check & p2Check & p3Check
  }
  
  def main(args: Array[String]): Unit = {

    val x = Integer.parseInt("1011010", 2)

    // val distances = 0 to 7
    // val all = distances.map(d => closeBitstrings(x,d)).flatten.filter(c=>isValidCodeword(c))
    // println("All codewords:")
    // all foreach(s => println(toBinary(s,7)))
    
    val dist = 3
    println("Codeword is " + toBinary(x, 7))
    val res = closeBitstrings(x, dist).sorted.filter(c => isValidCodeword(c))
    println("There are " + res.length.toString + " codewords of Hamming distance equal " + dist.toString)
    res.foreach(x => println(toBinary(x, 7)))
  }
}
/*
    Output:
        Codeword is 1011010
        There are 7 codewords of Hamming distance equal 3
        0010011
        0011100
        0101010
        1000110
        1001001
        1110000
        1111111
*/