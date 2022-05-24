import scala.util.Random

object hammingWeight {

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val millis = (System.nanoTime - now) / 1000000
    println("%d milliseconds".format(millis))
    result
  }

  def hamming(v: Int): Int = {
    var cnt = 0
    var tmp = v
    while (tmp != 0) {
      // println(tmp.toBinaryString)
      tmp = tmp & (tmp - 1)
      cnt = cnt + 1
    }
    cnt
  }

  def hamming2(v: Int): Int = {
    v.toBinaryString.foldLeft(0)((c, v) =>
      if (v == '1') { c + 1 }
      else { c }
    )
  }

  def rep(n: Int): Unit ={

    var cnt = n
    while(cnt > 0)
    {
        hamming(Random.nextInt().abs)
        cnt = cnt - 1
    }

  }

  def rep2(n: Int): Unit ={

    var cnt = n
    while(cnt > 0)
    {
        hamming2(Random.nextInt().abs)
        cnt = cnt - 1
    }

  }


  def main(args: Array[String]): Unit = {

    val x = 1729 * 1729
    println(x.toBinaryString)
    println(hamming(x))
    println(hamming2(x))
    time(rep(10000000))
    time(rep2(10000000))
  }
}

/*
  Output:
    1011011001110110000001
    11
    11
    330 milliseconds
    1700 milliseconds

*/