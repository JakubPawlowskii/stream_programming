import scala.util.Random._
import scala.math.exp

object Chernoff {

  def sumOfBernoullis(p: Double, n: Int): Int = {

    List
      .fill(n)(nextDouble())
      .map(x => {
        if (x < p) 1
        else 0
      })
      .sum
  }

  def main(args: Array[String]): Unit = {

    val n = 600
    val rep = 1000

    val p = 1.0 / 6.0
    val delta = 1.0 / 5.0

    val Xs = List.fill(rep)(sumOfBernoullis(p, n))

    val expectedValCalculated = Xs.sum.toDouble / rep
    val expectedValExact = n * p
    println("Estimated EX ≈ " + expectedValCalculated.toString)
    println("Exact EX = " + expectedValExact.toString)

    val a = (1 - delta) * expectedValExact
    val LHS = Xs.count(x => x <= a).toDouble / rep
    val RHS = exp(-0.5 * delta * delta * expectedValExact)

    println("LHS = " + LHS.toString)
    println("RHS = " + RHS.toString)
    println("Is LHS ≤ RHS?  " + (LHS <= RHS).toString)
  }
}
/*
Sample output:
    Estimated EX ≈ 99.902
    Exact EX = 100.0
    LHS = 0.014
    RHS = 0.13533528323661262
    Is LHS ≤ RHS?  true

 */
