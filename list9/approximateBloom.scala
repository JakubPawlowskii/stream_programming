import BloomFilter.BloomFilter.bloomFilter
import scala.io._
import java.io.FileNotFoundException

object approximateBloom {

  def main(args: Array[String]): Unit = {

    if (args.length > 0) {

      if (args.length > 1)
        println("To many arguments, discarding all except the first one.")
      val file =
        try {
          Source.fromFile(args(0))
          args(0)
        } catch {
          case e: FileNotFoundException => ""
        }

      if (file == "") {
        println("Invalid file path. Exiting...")
        System.exit(0)
      }

      val source = Source.fromFile(file)
      val words = source.mkString.split("\\W+").map(n => n.toLowerCase)
      source.close
      val exact = words.distinct.length
      println(
        "Exact number of distinct words in " + file + " is " + words.distinct.length
      )

      val bloom = bloomFilter(17,6)
      bloom.initializeFilter(words)
      println(
        "Approximate number of distinct words in " + file + " is " + bloom.cardinality().round
      )


    }
  }
}

/*

Input:  scala approximateBloom canterbury-corpus/canterbury/alice29.txt                                                                              
Output:  
        Exact number of distinct words in canterbury-corpus/canterbury/alice29.txt is 2580
        Approximate number of distinct words in canterbury-corpus/canterbury/alice29.txt is 2560

Input:  scala approximateBloom canterbury-corpus/large/bible.txt                                                                                      
Output:
        Exact number of distinct words in canterbury-corpus/large/bible.txt is 12473
        Approximate number of distinct words in canterbury-corpus/large/bible.txt is 12194

*/
