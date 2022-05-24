object curryUncurry{

    def curry[A,B,C](f: (A,B) => C): A => (B=>C) = (x:A)=>f(x,_:B)
    def uncurry[A,B,C](f: A => B => C): (A,B) => C = (x:A,y:B) => f ( x ) ( y )

    def main(args: Array[String]): Unit = {

        val f: (Double, Double) => Double = (x:Double,y:Double) => x + y*y
        println(f(1,2))
        println(curry(f)(1)(2))
        println(f(1,2) == curry(f)(1)(2))
        println(uncurry(curry(f))(3,4) == f(3,4) )

        val g: (String,Int) => String = (x:String, n:Int) => List.fill( n )(x).mkString 
        println(g("la",3))
        println(curry(g)("la")(3))
        println(uncurry(curry(g))("la",3) == g("la",3))        

    }
}