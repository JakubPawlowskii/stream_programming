object tailFibonacci {
  import scala.annotation.tailrec

@tailrec
def tailFib(n:Int, a:BigInt = 0, b:BigInt = 1 ): BigInt = n match {
    case 0 => a
    case 1 => b
    case _ => tailFib(n - 1, b, a + b)
}

  def main(args: Array[String]): Unit = {

    println(tailFib(9)) // 34
    println(tailFib(100)) // 354224848179261915075
    println(tailFib(100000)) // still computes easily, but the result is far too long to paste it here

  }
}
