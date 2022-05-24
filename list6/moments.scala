import scala.util.{Try, Success, Failure}
import scala.io._
import scala.math.pow

object moments {

  def moment(words: Array[String], k: Int): Int = {
    words
      .groupBy(identity)
      .map(n => n._2.length)
      .map(n => pow(n, k).intValue)
      .sum
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

    val source = Source.fromFile(filename)
    val words = source.mkString.split("\\W+").map(n => n.toLowerCase)
    source.close

    val moments = List(0, 1, 2, 3).map(n => (n, moment(words, n)))
    moments foreach println

    println("There are " + words.length + " words")
    println("There are " + words.distinct.length + " distinct words")

    /*
      Example output on alice29.txt
      (0,2580)
      (1,27334)
      (2,7648302)
      (3,339434667)
      There are 27334 words
      There are 2580 distinct words

      So the 0-th moment is the number of distinct words in a stream, whereas 1-st moment is the number of all words

     */

    val zerothMoment = moments(0)._2
    val firstMoment = moments(1)._2
    val secondMoment = moments(2)._2

    /*
        Minium of second moment can be shown via Lagrange multipliers to be given by the formula below.
        However, this function does not attain a true maximum (is a multidimensional paraboloid), hence
        the maximal value under constraints imposed by the zeroth and first moments, is somewhere on the boundary
        of the set given by constraints 0<mi<n1. It turns out that maximum corresponds to setting 
        mi for a single i to be equal to n1 and the rest to 0.
     */
    val secondMomentMinimum = (firstMoment * firstMoment) / zerothMoment
    val secondMomentMaximum = firstMoment * firstMoment

    println(
      secondMomentMinimum.toString + " ≤ " + secondMoment.toString + " ≤ " + secondMomentMaximum.toString
    )
    /*
      Output:
        289592 ≤ 7648302 ≤ 747147556
    */
  }
}
