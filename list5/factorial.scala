import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

object factorial {

  def tailFactorial(n: BigInt): BigInt = {
    @tailrec
    def go(acc: BigInt, n: BigInt): BigInt = {
      if (n <= 1) acc
      else go(n * acc, n - 1)
    }
    go(1, n)
  }
  
// @tailrec
/*
factorial.scala:19: error: could not optimize @tailrec annotated method factorial: it contains a recursive call not in tail position
    else n * factorial(n - 1)

We multiply the returned value by n, so the call is not in tail position - hence the conversion to iterative loops is impossible and we get the error
*/

  def factorial(n: BigInt): BigInt = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }

  def main(args: Array[String]): Unit = {

    val n = Try(args(0).toInt) match {
      case Success(n) => n
      case Failure(exception) => {
        println(exception)
        println("Calculating factorial(0).")
        0
      }
    }

    if (n < 0) {
      println("Input cannot be a negative BigInteger.")
      println("Calculating factorial(0).")

    }
    // println(factorial(n))
    println(tailFactorial(n))

    /*
      Depending on the current usage of memory by other processes running on my computer, I was able to calculate
      factorial(n) up to n = 5700

      Using the tail recursive tailFactorial(n), I checked values of n up to 500000 and obtained results without any problems.
    */
  }
}
