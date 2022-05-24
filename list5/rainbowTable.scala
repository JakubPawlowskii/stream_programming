import scala.util.hashing.MurmurHash3

object rainbowTable {

  /*
        ASCII alphanumeric and some common special characters have decimal representations
        from 33 to 126, so ideally we should convert hash to a list of 4 integers from 33 to 126 (0 to 93)

        reduce(hash: Int): String takes a hash, converts it into binary representation, which has 32
        bits because that's how MurmurHash3 works. Then it splits it into substrings of length 7, and take four leftmost
        such substrings. The length is chosen to be 7 so as to correspond to a binary number from range 0 to 127.
        Then, we apply the shift(n: Int): Int method, so as to guarantee that our number is in the range 33 to 126.
        Finally they are converted to Chars and combined into a string.

   */

  def toBinary(i: Int, digits: Int) =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')

  def shift(n: Int): Int = {
    if (n < 33) n + 33
    else if (n == 127) n - 1 // avoiding DEL character with code 127
    else n
  }
  def parseInt(s: String): Int = {
    Integer.parseInt(s, 2)
  }
  def reduce(hash: Int): String = {
    toBinary(hash, 32)
      .grouped(7)
      .toList
      .reverse
      .takeRight(4)
      .map(parseInt _ andThen shift _)
      .map(_.toChar)
      .mkString
  }

  def nextElement(n: Int): (String, Int) = {
    val str = reduce(n)
    val new_hash = MurmurHash3.stringHash(str)
    (str, new_hash)
  }
  def rainbow(s: String): LazyList[(String, Int)] = {

    LazyList.iterate((s, MurmurHash3.stringHash(s)))(p => nextElement(p._2))

  }

  def main(args: Array[String]): Unit = {

    lazy val rTab = rainbow("AAAA")

    val (password, hash) = rTab(100)
    println(password) // p'ro
    println(hash)     // 330460053
    println(MurmurHash3.stringHash(password)) // 330460053
  }
}
