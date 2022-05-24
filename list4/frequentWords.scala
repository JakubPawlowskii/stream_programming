import java.io.FileNotFoundException
object frequentWords {

  import scala.io._

  def parseArgs(args: Array[String]): (String, Int) = {
    if (args.length > 1) {

      if (args.length > 2)
        println("To many arguments, discarding all except the first two ones.")
      val file =
        try {
          Source.fromFile(args(0))
          args(0)
        } catch {
          case e: FileNotFoundException => ""
        }
      val k =
        try {
          args(1).toInt
        } catch {
          case e: NumberFormatException => 0
        }
      (file, k)
    } else {
      println("Not enough arguments.")
      ("", 0)
    }
  }

  def main(args: Array[String]): Unit = {

    val (file, k) = parseArgs(args)
    if (file == "") {
      println("Invalid file path. Exiting...")
      System.exit(0)
    }
    if (k <= 0) {
      println("Invalid number of most common words. Exiting...")
      System.exit(0)
    }

    val source = Source.fromFile(file)
    val words = source.mkString.split("\\W+").map(_.toLowerCase)
    source.close

    val kMostFrequentWords = words
      .groupBy(identity)
      .toList
      .map(n => (n._1, n._2.size))
      .sortBy(_._2)
      .takeRight(k)
      .reverse
    kMostFrequentWords foreach { println }
  }

  /*
  10 most frequent words in alice29.txt
  (the,1642)
  (and,872)
  (to,729)
  (a,632)
  (it,595)
  (she,552)
  (i,543)
  (of,513)
  (said,462)
  (you,411)
   */

}
