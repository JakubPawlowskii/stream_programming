import java.io.FileNotFoundException

object majority {

  def majority(stream: Iterable[String]): (String, Int) = {
    stream.foldLeft(("", 0))((c, v) => {
      if (c._2 == 0) {
        println("T = " + v + ", n = 1")
        (v, 1)
      } else {
        if (c._1 == v) {
          println("T = " + c._1 + ", n = " + (c._2 + 1).toString)
          (c._1, c._2 + 1)
        } else {
          println("T = " + c._1 + ", n = " + (c._2 - 1).toString)
          (c._1, c._2 - 1)
        }

      }
    })
  }

  def majority(
      stream: Iterable[String],
      majorityElement: String
  ): (String, Int) = {
    stream.foldLeft(("", 0))((c, v) => {

      if (c._2 == 0) {
        if (majorityElement == c._1) println("T = " + v + ", n = 1, n' = 1" )
        else println("T = " + v + ", n = 1, n' = -1" )
        (v, 1)
      } else {
        if (c._1 == v) {
          if (majorityElement == c._1) println("T = " + c._1 + ", n = " + (c._2 + 1).toString + ", n' = " + ((c._2+1)).toString)
          else println("T = " + c._1 + ", n = " + (c._2 + 1).toString + ", n' = " + (-(c._2+1)).toString)
          (c._1, c._2 + 1)
        } else {
          if (majorityElement == c._1) println("T = " + c._1 + ", n = " + (c._2 - 1).toString + ", n' = " + ((c._2-1)).toString)
          else println("T = " + c._1 + ", n = " + (c._2 - 1).toString + ", n' = " + (-(c._2-1)).toString)
          (c._1, c._2 - 1)
        }

      }
    })
  }

  def main(args: Array[String]): Unit = {

    val stream =
      List("a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "c", "c", "c")

    println("Running the majority algorithm: ")
    val majorityElement = majority(stream)
    println("Majority element is " + majorityElement)
    println("Verifying the results: ")
    majority(stream, majorityElement._1)

  }

}

/*
Output:

T = a, n = 3
T = a, n = 2
T = a, n = 1
T = a, n = 0
T = b, n = 1
T = b, n = 0
T = c, n = 1
T = c, n = 2
T = c, n = 1
T = c, n = 2
T = c, n = 3
Majority element is (c,3)
Verifying the results: 
T = a, n = 1, n' = -1
T = a, n = 2, n' = -2
T = a, n = 3, n' = -3
T = a, n = 2, n' = -2
T = a, n = 1, n' = -1
T = a, n = 0, n' = 0
T = b, n = 1, n' = -1
T = b, n = 0, n' = 0
T = c, n = 1, n' = -1
T = c, n = 2, n' = 2
T = c, n = 1, n' = 1
T = c, n = 2, n' = 2
T = c, n = 3, n' = 3
*/
