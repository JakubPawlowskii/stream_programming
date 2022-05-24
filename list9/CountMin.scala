package CountMin {
  import scala.util.hashing.MurmurHash3
  import scala.math.{exp, pow, ceil, log}
  object CountMin {

    def countMin(m: Int, k: Int) = new CountMin(m, k)
    
  }
  class CountMin(private val m: Int, private val k: Int) {
    
    val M = pow(2, m).toInt
    var bitArrays: Array[Array[Int]] = Array.fill[Array[Int]](k)(Array.fill(M)(0))

    def get_k(): Int = k
    def get_m(): Int = m

    def add(s: String): Unit = {
      val hashes = familyHash(s, k)
      var i = 0
      hashes.foreach(h => {bitArrays(i)(h) = bitArrays(i)(h) + 1; i = i + 1})
    }

    def initialize(stream: Iterable[String]): Unit = {
      stream.foreach(s => add(s))
    }

    def frequency(s: String): Int = {
      val hashes = familyHash(s,k)
      var i = -1
      val frequencies = hashes.map(h => {i = i + 1; bitArrays(i)(h)})
      frequencies.min
    }

    private val mask = ((1 << m) - 1)
    private def familyHash(s: String, k: Int): List[Int] = {
      val hs: Int = MurmurHash3.stringHash(s)
      val (high, low) = (hs >>> m, hs & mask)
      List.tabulate(k)(j => (high + j * low) & mask)
    }
  }

}

