import java.io.FileNotFoundException
object distinctWords {

  import scala.io._


  def main(args: Array[String]): Unit = {

    if (args.length > 0) {
      
      if (args.length > 1) println("To many arguments, discarding all except the first one.")
      val file = try{
        Source.fromFile(args(0))
        args(0)
      }
      catch
        {
         case e : FileNotFoundException => ""
        }

      if(file == "")
      {
        println("Invalid file path. Exiting...")
        System.exit(0)
      }

      val source = Source.fromFile(file)
      val words = source.mkString.split("\\W+").map(n => n.toLowerCase)
      source.close

      println("number of words in " + file + " is " + words.length)             // 27334 words
      println(
        "number of distinct words in " + file + " is " + words.distinct.length  // 2580 distinct words
      )

    } else println("No arguments provided.")
  }
}
