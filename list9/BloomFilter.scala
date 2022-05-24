package BloomFilter {
  import scala.util.hashing.MurmurHash3
  import scala.math.{exp, pow, ceil, log}
  object BloomFilter {

    def bloomFilter(m: Int, k: Int) = new BloomFilter(m, k)
    def bloomFilter(n: Int, error: Double) = {
      val m = ceil(log((-n * (log(error) / (pow(log(2), 2))))) / log(2)).toInt
      val k = ceil(-log(error) / log(2)).toInt
      new BloomFilter(m, k)
    }
    
  }
  class BloomFilter(private val m: Int, private val k: Int) {
    
    val M = pow(2, m).toInt
    var bitArray: Array[Boolean] = Array.fill[Boolean](M)(false)

    def get_k(): Int = k
    def get_m(): Int = m

    def add(s: String): Unit = {
      val hashes = familyHash(s, k)
      hashes.foreach(h => bitArray(h) = true)
    }

    def initializeFilter(stream: Iterable[String]): Unit = {
      stream.foreach(s => add(s))
    }

    def check(s: String): Boolean = {

      val hashes = familyHash(s, k)
      val hashesPresent = hashes.filter(h => bitArray(h)).length
      if (hashesPresent == k) true
      else false
    }

    def checkStream(s: List[String]): List[String] = {
      s.filter(n => check(n))
    }

    def cardinality(): Double = {
      val hamming = bitArray.foldLeft(0)((c, v) =>
      if (v) { c + 1 }
      else { c }
    )
      - M.toDouble/k * log( 1 - hamming.toDouble/M )
    }

    private val mask = ((1 << m) - 1)
    private def familyHash(s: String, k: Int): List[Int] = {
      val hs: Int = MurmurHash3.stringHash(s)
      val (high, low) = (hs >>> m, hs & mask)
      List.tabulate(k)(j => (high + j * low) & mask)
    }
  }

}

