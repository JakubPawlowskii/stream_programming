object hammingDistance{
    
    def hammingWeight(v: Int): Int = {
    var cnt = 0
    var tmp = v
    while (tmp != 0) {
      tmp = tmp & (tmp - 1)
      cnt = cnt + 1
    }
    cnt
  }
    def hammingDistance(a: Int, b: Int) : Int = {
        hammingWeight(a ^ b)
    }
    
    def main(args: Array[String]): Unit = {

        val a = 1
        val b = -1
        println(a.toString + " = " + a.toBinaryString )
        println(b.toString + " = " + b.toBinaryString )
        println("Hamming distance is " + hammingDistance(a,b))
    }
}
/*
  Output
    1729 = 11011000001
    2989441 = 1011011001110110000001
    Hamming distance is 10

*/
