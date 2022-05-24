object CountWords {
  
  import scala.io._
  
  def main(args: Array[String]): Unit = {
    
    if (args.length > 0) {
    
      val file = args(0)
      val fileSource = Source.fromFile(file)
      val words = fileSource.mkString.split("\\W+")
    
      fileSource.close
      println("number of words: " + words.length)
    
    } else println("no file")
  }
}
