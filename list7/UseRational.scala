object Assignment2 {
  import Rational._
  import Element._
  import scala.util.{Try, Success, Failure}

  def main(args: Array[String]): Unit = {

    val n = Try(args(0).toInt) match {
      case Success(n) => n
      case Failure(e) => {
        println(e)
        -1
      }
    }
    val d = Try(args(1).toInt) match {
      case Success(d) => d
      case Failure(e) => {
        -1
      }
    }

    if (n < 0) System.exit(1)
    if (d < 0) {
      println(elem(n, 1))
    } else
      println(elem(n, d))
  }

  /*
    Output:
      scala Assignment2 13 143
      13
      ---
      143

      scala Assignment2 314
      314

   */
}

object Assignment3 {
  import Rational._
  import Element._

  def main(args: Array[String]): Unit = {

    val lhs = elem(Rational(3, 5)) + elem(Rational(2, 14))
    val rhs = elem(Rational(3, 5) + Rational(2, 14))
    println(lhs equals rhs)

  }

  /*
    Output:
      3   1   26
      - + - = --
      5   7   35
   */
}
