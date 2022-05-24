import scala.math.pow
import scala.math.floorMod
import scala.util.Random

class stochasticCounter(private val m: Int) {

  require(m > 0 && m % 2 == 0)
  private var counters: Array[Int] = Array.fill[Int](m)(0)

  private def rho(n: Int)   = Integer.numberOfLeadingZeros(n) + 1
  private def index(n: Int) = floorMod(n, m)

  def add(n: Int): Unit = {
    val r = rho(n)
    val i = index(n)
    if (r > counters(i)) counters(i) = r
  }

  def getCountArithmetic(): BigInt = {
    counters.map(n => BigInt(pow(2, n).intValue)).sum
  }
  def getCountHarmonic(): Int = {

      val cnt = m * m * (1.0 / (counters.map(n => 1.0 / pow(2, n)).sum))
      cnt.toInt
    
  }

  def getCounters(): Array[Int] = counters
  def getCounter(idx: Int): Int = counters(idx)
  def reset(): Unit             = counters = Array.fill[Int](m)(0)

}

object betterStochasticCounter {

  def main(args: Array[String]): Unit = {

    println(
      "Comparing stochasticCounter against exact number of distinct elements for a relatively small stream of 2^20 random integers:"
    )
    val m       = 256
    val counter = new stochasticCounter(m)

    val randomList = List.fill(pow(2, 20).intValue)(Random.nextInt())
    randomList.foreach(k => counter.add(k))

    println(
      "Exact number of distinct elements                      is " + randomList.distinct.length.toString)
    println(
      "Estimation with stochastic counter and arithmetic mean is " + counter
        .getCountArithmetic()
        .toString
    )
    println(
      "Estimation with stochastic counter and harmonic   mean is " + counter
        .getCountHarmonic()
        .toString
    )

    println("Now comparing harmonic and arithmetic means on a larger stream of 2^28 elements (does not fit in memory):")
    counter.reset()
    var k = pow(2,28).intValue
    while(k > 0)
    {
      counter.add(Random.nextInt())
      k = k - 1
    }
    println(
      "Estimation with stochastic counter and arithmetic mean is " + counter
        .getCountArithmetic()
        .toString
    )
    println(
      "Estimation with stochastic counter and harmonic   mean is " + counter
        .getCountHarmonic()
        .toString
    )
    /*
      Example output with m = 256 buckets:
        Comparing stochasticCounter against exact number of distinct elements for a relatively small stream of 2^20 random integers:
        Exact number of distinct elements                      is 1048448
        Estimation with stochastic counter and arithmetic mean is 16028672
        Estimation with stochastic counter and harmonic   mean is 1524719
        Now comparing harmonic and arithmetic means on a larger stream of 2^28 elements (does not fit in memory):
        Estimation with stochastic counter and arithmetic mean is 1828716544
        Estimation with stochastic counter and harmonic   mean is 342110672
    
    */
  }
}
