import scala.util.Random._

object BienaymeChebyshev{
    def main(args: Array[String]): Unit = {
        
        val n = 1000000
        val rep = 1000

        val k = 3.0
        val sigma = 1.0 // mu = 0.0
        val ksigma = k*sigma


        var p = 0.0
        for (i <- Range(0,rep))
        {
            val cnt = List.fill(n)(nextGaussian().abs).count(x => x >= ksigma).toDouble
            p += cnt/n
        }
        p = p / rep

        println("LHS = " + p)
        println("RHS = " + 1/(k*k))
        println("Is LHS ≤ RHS?  " + (p <= 1/(k*k)).toString)

    }
}
/*
    Output:
        LHS = 0.0027032759999999975
        RHS = 0.1111111111111111
        Is LHS ≤ RHS?  true
*/