object MultPoly {

import polynomialGF2.polynomialGF2._
import scala.util.Random.nextInt

def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val millis = (System.nanoTime - now) / 1000000
    println("%d milliseconds".format(millis))
    result
  }

def multiply1(p1: BigInt, p2: BigInt, n:Int): BigInt = {
    var N = n
    val p1poly = polynomialGF2(p1)
    val p2poly = polynomialGF2(p2)
    while(N >= 1)
    {
        p1poly * p2poly
        N = N - 1
    }
    (p1poly*p2poly).rep
}
def multiply2(p1: BigInt, p2: BigInt, n:Int): BigInt = {
    var N = n
    val p1poly = polynomialGF2(p1)
    val p2poly = polynomialGF2(p2)
    while(N >= 1)
    {
        p1poly ** p2poly
        N = N - 1
    }
    (p1poly**p2poly).rep
}
def multiply3(p1: BigInt, p2: BigInt, n:Int): BigInt = {
    var N = n
    val p1poly = polynomialGF2(p1)
    val p2poly = polynomialGF2(p2)
    while(N >= 1)
    {
        p1poly *** p2poly
        N = N - 1
    }
    (p1poly***p2poly).rep
}


def main(args: Array[String]): Unit = {
    val p1 = nextInt(1024) + 1
    val p2 = nextInt(1024) + 1
    println(polynomialGF2(p1))
    println(polynomialGF2(p2))
    println(time(multiply1(p1,p2, 10000))) // * is the multiplication with while loop
    println(time(multiply2(p1,p2, 10000))) // ** is the recursive multiplication from statement of assignment 3
    println(time(multiply3(p1,p2, 10000))) // *** is the tail recursive version
    
}

}

/*
    Example output:
        x^7 + x^6 + x^2 + 1
        x^9 + x^8 + x^7 + x^6 + x^5 + x^3 + x + 1
        50 milliseconds
        66311
        32 milliseconds
        66311
        19 milliseconds
        66311

    We see than the tail recursive multiplication seems to be the fastest.
*/