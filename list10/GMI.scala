import scala.math.{cbrt, sqrt, pow}
import scala.util.Random

object GMI {
  def median(seq: Seq[Double]): Double = {
    val sortedSeq = seq.sorted
    if (seq.size % 2 == 1) sortedSeq(sortedSeq.size / 2)
    else {
      val (up, down) = sortedSeq.splitAt(seq.size / 2)
      (up.last + down.head) / 2
    }
  }
  def main(args: Array[String]): Unit = {

    val n = pow(2, 20).toInt + 1
    val random = List.fill(n)(Random.nextInt(Int.MaxValue).toDouble)
    val med = median(random)
    val arithmetic = random.sum / random.length
    val quadratic = sqrt(random.map(n => n*n).sum/ random.length)
    val cubic = cbrt(random.map(n => n*n*n).sum/ random.length)


    println("Median is " + med.toString)
    println("Arithmetic mean is " + arithmetic.toString)
    println("Quadratic mean is " + quadratic.toString)
    println("Cubic mean is " + cubic.toString)

    println("M1 ≤ M2 ≤ M3")
    println(arithmetic.toString+ " ≤ " + quadratic.toString + " ≤ " + cubic.toString)
  }
}

/*
    Output: 
        Median is 1.071357727E9
        Arithmetic mean is 1.0724286113225933E9
        Quadratic mean is 1.2388897638698518E9
        Cubic mean is 1.3521004358610446E9
        
        Arithmetic           ≤ Quadratic            ≤ Cubic
        1.0724286113225933E9 ≤ 1.2388897638698518E9 ≤ 1.3521004358610446E9

*/