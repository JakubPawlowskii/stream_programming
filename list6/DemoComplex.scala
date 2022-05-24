object DemoComplex{

    import Complex._

    def main(args: Array[String]):Unit = {
        
        val i = Complex(0,1)
        
        val a = Complex(1,1)
        val b = Complex(2,-2)
        val c = Complex(10)
        println("a = " + a)
        println("b = " + b)
        println("a + b = " + (a+b))
        println("a - b = " + (a-b))
        println("-a = " + -a)
        println("a* = " + a.conj)
        println("a + a* = " + (a+a.conj))
        println("|a| = " + a.modulus)
        println("a * a* = " + (a*a.conj))
        println("c = " + c)
        println("c*i = " + c*i)
        println("1/a = " + a.inverse)
        println("b/a = " + b/a )
        println("2 * b/a = " + (2 * b/a) )
        println("b/a * 2 = " + (b/a * 2) )

        val d = 2.0 + 4.0*i
        println("d = " + d)

        /*
        Output:
            a = 1.0 + 1.0i
            b = 2.0 - 2.0i
            a + b = 3.0 - 1.0i
            a - b = -1.0 + 3.0i
            -a = -1.0 - 1.0i
            a* = 1.0 - 1.0i
            a + a* = 2.0
            |a| = 1.4142135623730951
            a * a* = 2.0
            c = 10.0
            c*i = 10.0i
            1/a = 0.5 - 0.5i
            b/a = -2.0i
            2 * b/a = -4.0i
            b/a * 2 = -4.0i
            d = 2.0 + 4.0i
        */
        
    }
}