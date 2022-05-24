object amicableNumbers2 {

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val millis = (System.nanoTime - now) / 1000000
    println("%d milliseconds".format(millis))
    result
  }

  // sum of proper divisors of integer n
  def d(n: Int): Int = {
    List(1 to n / 2: _*).filter(x => (n % x) == 0).sum
  }

  // list of amicable pairs
  def amicableNumbers(n: Int): List[(Int, Int)] = {

    val a = List(1 to n: _*).map(n => (n, d(n)))
    a.filter(n => n._1 < n._2 && a.contains((n._2, n._1)))

  }
  //sum over the amicable numbers
  def amicableNumbersSum(n: Int): Int = {
    amicableNumbers(n).map(n => n._1 + n._2).sum
  }

  def main(args: Array[String]): Unit = {

    println(time(amicableNumbersSum(1))) // res = 0 in 2ms
    println(time(amicableNumbersSum(10))) // res = 0 in 1ms
    println(time(amicableNumbersSum(100))) // res = 0 in 3ms
    println(time(amicableNumbersSum(1000))) // res = 504 in 64ms
    println(time(amicableNumbersSum(10000))) // res = 31626 in 825ms
    println(time(amicableNumbersSum(100000))) // res = 852810 in 83106ms
    /*
      We see that the runtime grows very rapidly with growing m.
    */
  }

}
