object fibonacci {

  lazy val fibs: LazyList[BigInt] = {
    BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map(n => n._1 + n._2)
  }

  def indexFib(d: Int): Int = {

    fibs.takeWhile(_.toString.length < d).length

  }
  def main(args: Array[String]): Unit = {

    println(indexFib(2))                // 7
    println(indexFib(3))                // 12
    println(indexFib(4))                // 17
    println(indexFib(1000))             // 4782
    println(fibs(4781).toString.length) // 999
    println(fibs(4782).toString.length) // 1000

  }
}
