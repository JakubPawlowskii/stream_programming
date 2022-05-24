import scala.util.{Try, Success, Failure}
import scala.io._
import scala.util.hashing.MurmurHash3
object murmurHash {

  def main(args: Array[String]): Unit = {

    val filename: String = Try(Source.fromFile(args(0))) match {
      case Success(n) => {
        args(0)
      }
      case Failure(exception) => {
        println(exception)
        println("Invalid filename. Exiting...")
        System.exit(0)
        ""
      }
    }
    val source = Source.fromFile(filename)
    val fullText = source.mkString
    source.close

    val targetHash = 320915200
    val phraseLength = 24

    val targetPhrase = fullText.sliding(phraseLength,1).find(s => MurmurHash3.stringHash(s) == targetHash) match {
        case None => ""
        case s: Some[String] => s.get 
    }

    if(targetPhrase == "")
    {
        println("No phrase of length " + phraseLength.toString + ", with MurmurHash3 equal to " + targetHash.toString + " found in " + filename)
    }
    else{
        println("Phrase of length " + phraseLength.toString + ", with MurmurHash3 equal to " + targetHash.toString + " found in " + filename)
        println("It is '" + targetPhrase + "'")
        println("Hash check : " + MurmurHash3.stringHash(targetPhrase))
    }
  }

  /*
    Sample outputs:
    > scala murmurHash canterbury-corpus/canterbury/alice29.txt   
    > Phrase of length 24, with MurmurHash3 equal to 320915200 found in canterbury-corpus/canterbury/alice29.txt
      It is 'Curiouser and curiouser!'
      Hash check : 320915200

    > scala murmurHash canterbury-corpus/canterbury/asyoulik.txt  
    > No phrase of length 24, with MurmurHash3 equal to 320915200 found in canterbury-corpus/canterbury/asyoulik.txt
    
  */
}
