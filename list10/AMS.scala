import scala.util.{Try, Success, Failure}
import scala.io._
import scala.math.{pow}
import scala.util.Random

object AMS {

  def momentExact(words: Array[String], k: Int): Long = {
    words
      .groupBy(identity)
      .map(n => n._2.length)
      .map(n => pow(n, k).round)
      .sum
  }

  def momentApproximate(words: Array[String], k: Int, r: Int): Long = {

    val m = words.length

    var expectedVal: Double = 0

    for (n <- Range(0, r)) {

      val i = Random.nextInt(m)
      val elem = words(i)
      val c = words.takeRight(m - i).count(s => s == elem)
      expectedVal += pow(c, k) - pow(c - 1, k)
    }

    expectedVal = expectedVal / r
    (m * expectedVal).round


  }

  def main(args: Array[String]): Unit = {

    val filename = Try(Source.fromFile(args(0))) match {
      case Success(n) => args(0)
      case Failure(exception) => {
        println(exception)
        println("Invalid filename. Exiting...")
        System.exit(1)
        ""
      }
    }

    val avg = Try(args(1).toInt) match {
      case Success(n) => n
      case Failure(exception) => {
        println(exception)
        println("Invalid number of repetitions. Exiting...")
        System.exit(1)
        0
      }
    }

    val source = Source.fromFile(filename)
    val words = source.mkString.split("\\W+").map(n => n.toLowerCase)
    source.close

    val moments = List(2, 3).map(n => (n, momentExact(words, n)))
    println("Exact moments: ")
    moments foreach println

    val momentsApprox =
      List(2, 3).map(n => (n, momentApproximate(words, n, avg)))
    println("Approximate moments: ")
    momentsApprox foreach println
  }
}

/*

Input: scala AMS canterbury-corpus/canterbury/alice29.txt 60 
Output: 
    Exact moments: 
    (2,7648302)
    (3,6914019604)
    Approximate moments: 
    (2,6653096)
    (3,7487405815)
Input: scala AMS canterbury-corpus/canterbury/alice29.txt 10000
Output: 
    Exact moments: 
    (2,7648302)
    (3,6914019604)
    Approximate moments: 
    (2,7812276)
    (3,7125316384)
*/