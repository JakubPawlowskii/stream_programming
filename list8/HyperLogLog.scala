import scala.math.{floor, ceil, pow, log, max}
import scala.util.hashing.MurmurHash3

package HyperLogLog {

  object HyperLogLog {

    class StringHLL(b: Int, corrections: Boolean)
        extends HyperLogLog[String](b, corrections) {
      override def hash(s: String): Int = MurmurHash3.stringHash(s)
    }
    class IntHLL(b: Int, corrections: Boolean)
        extends HyperLogLog[Int](b, corrections) {
      override def hash(s: Int): Int = s
    }

    def stringCounter(b: Int, corrections: Boolean) = new StringHLL(b, corrections)
    def intCounter(b: Int, corrections: Boolean) = new IntHLL(b, corrections)

  }

  abstract class HyperLogLog[T](
      private val b: Int,
      private val corrections: Boolean
  ) {

    require(b > 0)

    private val m: Int = pow(2, b).intValue
    private val alpha: Double = m match {
      case 16 => 0.673
      case 32 => 0.697
      case 64 => 0.709
      case _  => 0.7213 / (1 + 1.079 / m.toDouble)
    }
    private var counters: Array[Byte] = Array.fill[Byte](m)(0)

    def hash(s: T): Int

    private def toBinary(i: Int, digits: Int) =
      String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')

    private def rho(n: Int): Byte = {
      (Integer.numberOfLeadingZeros(n) + 1 - b).toByte
    }

    private def split(n: Int): (Int, Int) = {
      (n >>> (32 - b), (((1 << (32 - b)) - 1) & n))
      // val bin = toBinary(n, 32)
      // val index = Integer.parseInt(bin.take(b), 2)
      // val value = Integer.parseInt(bin.takeRight(bin.length - b), 2)
      // (index, value)
    }

    def add(s: T): Unit = {
      val (index, value) = split(hash(s))
      counters(index) = max(counters(index), rho(value)).toByte
    }

    def addStream(stream: Iterable[T]): Unit = {
      stream.foreach(n => add(n))
    }

    def smallRangeCorrection(count: Double): Double = {
      val v = counters.filter(n => n == 0).length
      if (v != 0) {
        // println("Small range corrections")
        m * log(m.toDouble / v.toDouble)
      } else {
        count
      }
    }

    def largeRangeCorrection(count: Double): Double = {
      // println("Large range corrections")
      (-pow(2, 32) * log(1 - count / pow(2, 32)))
    }

    def getCount(): Double = {
      val count =
        (alpha * ((m * m).toDouble / (counters.map(n => pow(2, -n)).sum)))
      if (corrections) {
        if (count <= 2.5 * m) smallRangeCorrection(count)
        else if (count > (1.0 / 30.0) * pow(2, 32)) largeRangeCorrection(count)
        else count

      } else count

    }

    def getCounters(): Array[Byte] = counters
    def getCounter(idx: Int): Byte = counters(idx)
    def reset(): Unit = counters = Array.fill[Byte](m)(0)

  }

}
