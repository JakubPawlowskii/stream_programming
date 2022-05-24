import HyperLogLog.HyperLogLog.stringCounter
import scala.io._
import java.io.FileNotFoundException

object approximateWords {

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
        "exact number of distinct words in " + file + " is " + words.distinct.length
      )

      val counter = stringCounter(10, true)
      counter.addStream(words)
      var count = counter.getCount()
      var error = scala.math.abs(exact - count) / exact.toDouble
      error
      println(
        "number of distinct words approximated via HLL with 2^10 buckets in " + file + " is " + count.toString + " , error is " + error.toString
      )

      val counter2 = stringCounter(12, true)
      counter2.addStream(words)
      count = counter2.getCount()
      error = scala.math.abs(exact - count) / exact.toDouble

      println(
        "number of distinct words approximated via HLL with 2^12 buckets in " + file + " is " + count.toString + " , error is " + error.toString
      )

      val counter3 = stringCounter(14, true)
      counter3.addStream(words)
      count = counter3.getCount()
      error = scala.math.abs(exact - count) / exact.toDouble

      println(
        "number of distinct words approximated via HLL with 2^14 buckets in " + file + " is " + count.toString + " , error is " + error.toString
      )
      val counter4 = stringCounter(16, true)
      counter4.addStream(words)
      count = counter4.getCount()

      error = scala.math.abs(exact - count) / exact.toDouble

      println(
        "number of distinct words approximated via HLL with 2^16 buckets in " + file + " is " + count.toString + " , error is " + error.toString
      )

    } else println("No arguments provided.")

  }
}

/*
    Input: scala approximateWords canterbury-corpus/canterbury/alice29.txt
    Output:
        exact number of distinct words in canterbury-corpus/canterbury/alice29.txt is 2580
        number of distinct words approximated via HLL with 2^10 buckets in canterbury-corpus/canterbury/alice29.txt is 2724.098950682595 , error is 0.05585230646612207
        number of distinct words approximated via HLL with 2^12 buckets in canterbury-corpus/canterbury/alice29.txt is 2609.679315592838 , error is 0.011503610694898475
        number of distinct words approximated via HLL with 2^14 buckets in canterbury-corpus/canterbury/alice29.txt is 2581.0317205775273 , error is 3.998916967159949E-4
        number of distinct words approximated via HLL with 2^16 buckets in canterbury-corpus/canterbury/alice29.txt is 2578.049107248597 , error is 7.561599811639493E-4

    Input: scala approximateWords canterbury-corpus/large/bible.txt
    Output:
       exact number of distinct words in canterbury-corpus/large/bible.txt is 12473
       number of distinct words approximated via HLL with 2^10 buckets in canterbury-corpus/large/bible.txt is 12286.344300438112 , error is 0.014964779889512425
       number of distinct words approximated via HLL with 2^12 buckets in canterbury-corpus/large/bible.txt is 12409.790296578489 , error is 0.005067722554438485
       number of distinct words approximated via HLL with 2^14 buckets in canterbury-corpus/large/bible.txt is 12371.31132923837 , error is 0.008152703500491494
       number of distinct words approximated via HLL with 2^16 buckets in canterbury-corpus/large/bible.txt is 12514.259500612308 , error is 0.00330790512405262
 */
