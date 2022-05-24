object composition{
    
    import math._

    def cmp[A,B,C](f: B => C, g: A => B): A => C = (x: A) => f(g(x))
    
    def main(args: Array[String]): Unit = {
        
        val x = List.range(0,10).map(_.toDouble)

        val res1 = x.map(cmp(exp _,log _))
        val res2 = x.map(cmp(log _,exp _))
        println(res1)
        println(res2)

        val xxx = x.map(exp _ compose log _)
        val xx = x.map(exp _ andThen log _)
        println(xxx)
        println(xx)

        println(exp(log(3.0)))
        println(log(exp(3.0)))

        /*
            xx and xxx are not exactly equal because exp _ compose log _ gives exp(log)
            whereas exp _ andThen log _ gives log(exp)

            The order of composition matters in this case, probably because of some
            numerical details in implementations of log and exp functions in java math library.
        
        */

    }
}