import scala.math.pow
import scala.util.Random

class stochasticCounter() {

  private var M = 0
  private val rng = Random

  def add(): Unit = {
    val rho = Integer.numberOfLeadingZeros(rng.nextInt()) + 1
    if (rho > M) M = rho
  }

  def getCount(): BigInt = scala.math.pow(2, M).intValue
  def getM(): Int = M
  def reset(): Unit = M = 0
  
}

object stochasticCounter {

  def test(n: Int, k: Int): List[(Int, BigInt)] = {

    var tries = k
    var counts = List[(Int, BigInt)]()

    val counter = new stochasticCounter()
    while (tries > 0) {
      var i = 1
      while (i <= n) {
        counter.add()
        i += 1
      }
      counts = (counter.getM(), counter.getCount()) :: counts
      counter.reset()
      tries -= 1
    }
    counts
  }

  def main(args: Array[String]): Unit = {

    val n = pow(2, args(0).toInt).intValue
    val rep = args(1).toInt

    val counts = test(n, rep)
    val avg = (counts.map(_._1).sum.toDouble/counts.length, counts.map(_._2).sum/counts.length)
    
    println((args(0).toInt, n))
    println(avg)

    /*
      Sample outputs:
        1. for n = 2^20 and 100 repetitions
          (20,1048576)
          (21.44,12244746)
        2. for n = 2^22 and 100 repetitions
          (22,4194304)
          (23.42,39447429)
        3. for n = 2^24 and 100 repetitions
          (24,16777216)
          (25.39,155776450)
        4. for n = 2^24 and 100 repetitions
          (28,268435456)
          (29.45,958314577)
    */
  }

}
