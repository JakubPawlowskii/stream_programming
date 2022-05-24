import BloomFilter.BloomFilter.bloomFilter
import scala.io._
import java.io.FileNotFoundException
import scala.math.{pow, log, exp}
object approximateDifference {

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
    val wordsA = sourceA.mkString.split("\\W+").map(_.toLowerCase).toList
    val wordsAUnique = wordsA.distinct
    sourceA.close

    val sourceB = Source.fromFile(fileB)
    val wordsB = sourceB.mkString.split("\\W+").map(_.toLowerCase).toList
    val wordsBUnique = wordsB.distinct
    sourceB.close

    val inANotInB = wordsAUnique.filter(n => !wordsBUnique.contains(n))
    println("Words in " + fileA + " : " + wordsA.length)
    println("Words in " + fileB + " : " + wordsB.length)
    println("Unique words in " + fileA + " : " + wordsAUnique.length)
    println("Unique words in " + fileB + " : " + wordsBUnique.length)
    println(
      "There are exactly " + inANotInB.length.toString + " distinct words in " + fileA + ", that are not in " + fileB
    )

    val bloom = bloomFilter(wordsBUnique.length, 0.001)
    println("log2(m) = " + bloom.get_m())
    println("k = " + bloom.get_k())
    // val bloom = bloomFilter(16,4)
    bloom.initializeFilter(wordsB)
    val bloomInANotInB = wordsA.filter(s => !bloom.check(s)).distinct
    val estimate = bloomInANotInB.length
    val falsePositives =
      wordsAUnique
        .filter(s => bloom.check(s))
        .filter(s => !wordsBUnique.contains(s))
    println(
      "There are approximately " + estimate.toString + " distinct words in " + fileA + ", that are not in " + fileB
    )
    println("Number of false positives = " + falsePositives.length)
    println(
      "Actual rate of false positives is " + (falsePositives.length.toDouble / wordsAUnique.length.toDouble).toString
    )
    val falsePositiveTheoretical = pow(
      (1 - exp(
        -(wordsBUnique.length.toDouble * bloom.get_k().toDouble) / pow(
          2.0,
          bloom.get_m().toDouble
        )
      )),
      bloom.get_k().toDouble
    )
    println(
      "Theoretical rate of false positives is " + falsePositiveTheoretical.toString
    )
  }
}

/*  Results for fixed Bloom filter with m = 2^16 and k = 4 
    Input:  scala approximateDifference canterbury-corpus/large/bible.txt canterbury-corpus/canterbury/alice29.txt 
    Output:
      Words in canterbury-corpus/large/bible.txt : 767855
      Words in canterbury-corpus/canterbury/alice29.txt : 27334
      Unique words in canterbury-corpus/large/bible.txt : 12473
      Unique words in canterbury-corpus/canterbury/alice29.txt : 2580
      There are exactly 10939 distinct words in canterbury-corpus/large/bible.txt, that are not in canterbury-corpus/canterbury/alice29.txt
      There are approximately 10934 distinct words in canterbury-corpus/large/bible.txt, that are not in canterbury-corpus/canterbury/alice29.txt
      Number of false positives = 5
      Actual rate of false positives is 4.0086587027980436E-4
      Theoretical rate of false positives is 4.5062600370376286E-4
    
    Input:  scala approximateDifference canterbury-corpus/canterbury/alice29.txt canterbury-corpus/large/bible.txt        
    Output: 
      Words in canterbury-corpus/canterbury/alice29.txt : 27334
      Words in canterbury-corpus/large/bible.txt : 767855
      Unique words in canterbury-corpus/canterbury/alice29.txt : 2580
      Unique words in canterbury-corpus/large/bible.txt : 12473
      There are exactly 1046 distinct words in canterbury-corpus/canterbury/alice29.txt, that are not in canterbury-corpus/large/bible.txt
      There are approximately 958 distinct words in canterbury-corpus/canterbury/alice29.txt, that are not in canterbury-corpus/large/bible.txt
      Number of false positives = 88
      Actual rate of false positives is 0.034108527131782945
      Theoretical rate of false positives is 0.08066851563732258

    Results for Bloom filter optimized w.r.t. text length and supplied error rate 0.001
    Input: scala approximateDifference canterbury-corpus/large/bible.txt canterbury-corpus/canterbury/alice29.txt  
    Output: 
      Words in canterbury-corpus/large/bible.txt : 767855
      Words in canterbury-corpus/canterbury/alice29.txt : 27334
      Unique words in canterbury-corpus/large/bible.txt : 12473
      Unique words in canterbury-corpus/canterbury/alice29.txt : 2580
      There are exactly 10939 distinct words in canterbury-corpus/large/bible.txt, that are not in canterbury-corpus/canterbury/alice29.txt
      log2(m) = 16
      k = 10
      There are approximately 10938 distinct words in canterbury-corpus/large/bible.txt, that are not in canterbury-corpus/canterbury/alice29.txt
      Number of false positives = 1
      Actual rate of false positives is 8.017317405596088E-5
      Theoretical rate of false positives is 1.3321349474616426E-5

    Input: scala approximateDifference canterbury-corpus/canterbury/alice29.txt canterbury-corpus/large/bible.txt      
    Output:
      Words in canterbury-corpus/canterbury/alice29.txt : 27334
      Words in canterbury-corpus/large/bible.txt : 767855
      Unique words in canterbury-corpus/canterbury/alice29.txt : 2580
      Unique words in canterbury-corpus/large/bible.txt : 12473
      There are exactly 1046 distinct words in canterbury-corpus/canterbury/alice29.txt, that are not in canterbury-corpus/large/bible.txt
      log2(m) = 18
      k = 10
      There are approximately 1046 distinct words in canterbury-corpus/canterbury/alice29.txt, that are not in canterbury-corpus/large/bible.txt
      Number of false positives = 0
      Actual rate of false positives is 0.0
      Theoretical rate of false positives is 6.053379369586903E-5
*/
