import java.io.FileNotFoundException
import CountMin.CountMin.countMin
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

    val MostFrequentWords = words
      .groupBy(identity)
      .toList
      // .distinct
      .map(n => (n._1, n._2.size))
      .sortBy(_._1)

    val kMostFrequentWords = MostFrequentWords
      .sortBy(_._2)
      .takeRight(k)
      .reverse
    println(k.toString + " most frequent words: ")
    kMostFrequentWords foreach { println }

    val cnt = countMin(16, 4)
    cnt.initialize(words)
    val freqsAll = words.map(s => (s, cnt.frequency(s))).distinct.sortBy(_._1)

    println(
      k.toString + " most frequent words approximated with count-min sketch:"
    )
    freqsAll.sortBy(_._2).takeRight(k).reverse foreach println

    println("\nExact  | Count-Min sketch")
    val wrongs = MostFrequentWords
      .zip(freqsAll)
      .foldLeft(0)((cnt, s) => {
        if (s._1._2 == s._2._2) cnt
        else {
          println(s)
          cnt + 1
        }
      })
    println("There are " + wrongs.toString + " errors.")

  }

}

/*
  Input: scala approximateFrequencies.scala canterbury-corpus/large/bible.txt 10
  Output:
        10 most frequent words: 
        (the,61680)
        (and,49862)
        (of,33195)
        (to,13031)
        (that,12561)
        (in,12211)
        (he,9939)
        (shall,9733)
        (for,8821)
        (unto,8808)
        10 most frequent words approximated with count-min sketch:
        (the,61680)
        (and,49862)
        (of,33195)
        (to,13031)
        (that,12561)
        (in,12211)
        (he,9939)
        (shall,9733)
        (for,8821)
        (unto,8808)

        Exact  | Count-Min sketch
        ((abiezer,7),(abiezer,8))
        ((arvad,2),(arvad,6))
        ((attended,3),(attended,4))
        ((galal,3),(galal,4))
        ((letters,32),(letters,33))
        ((namely,23),(namely,25))
        ((silvanus,4),(silvanus,5))
        ((sorely,2),(sorely,3))
        ((valley,134),(valley,135))
        ((vomit,8),(vomit,10))
        ((zoar,10),(zoar,12))
        There are 11 errors.

  Input: scala approximateFrequencies.scala canterbury-corpus/canterbury/alice29.txt 10
  Output: 
        10 most frequent words: 
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
        10 most frequent words approximated with count-min sketch:
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

        Exact  | Count-Min sketch
        There are 0 errors.
      
 */
