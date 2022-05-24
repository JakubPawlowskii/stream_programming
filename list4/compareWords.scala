import java.io.FileNotFoundException
object compareWords {

  import scala.io._

  def parseArgs(args: Array[String]): (String, String) = {
    if (args.length > 1) {

      if (args.length > 2)
        println("To many arguments, discarding all except the first two ones.")
      val fileA =
        try {
          Source.fromFile(args(0))
          args(0)
        } catch {
          case e: FileNotFoundException => ""
        }
      val fileB =
        try {
          Source.fromFile(args(1))
          args(1)
        } catch {
          case e: FileNotFoundException => ""
        }

      (fileA, fileB)
    } else {
      println("Not enough arguments.")
      ("", "")
    }
  }

  def main(args: Array[String]): Unit = {

    val (fileA, fileB) = parseArgs(args)
    if (fileA == "") {
      println("Invalid first file path. Exiting...")
      System.exit(0)
    }
    if (fileB == "") {
      println("Invalid second file path. Exiting...")
      System.exit(0)
    }

    val sourceA = Source.fromFile(fileA)
    val wordsAUnique = sourceA.mkString.split("\\W+").map(_.toLowerCase).distinct
    sourceA.close

    val sourceB = Source.fromFile(fileB)
    val wordsBUnique = sourceB.mkString.split("\\W+").map(_.toLowerCase).distinct
    sourceB.close

    val inANotInB = wordsAUnique.filter(n => !wordsBUnique.contains(n))

    inANotInB foreach {println}
    println("There are " + inANotInB.length.toString + " distinct words in " + fileA + ", that are not in " + fileB)

    /*
      There are 1046 distinct words in canterbury-corpus/canterbury/alice29.txt, that are not in canterbury-corpus/large/bible.txt
      There are 10939 distinct words in canterbury-corpus/large/bible.txt, that are not in canterbury-corpus/canterbury/alice29.txt
    */

  }

}
