import scala.util.Random._
import scala.math._

object Task4 {

    def median(S: List[Double]): Double = {
        val l = S.length
        if (math.floorMod(l, 2) == 0) {
            0.5*(S(l/2) + S(l/2 - 1)) 
        } else {
            S((l-1)/2)
        }
    }

    def bernoulliX(n: Int, pInverse: Int): Int = {
        val v=List.fill(n)(math.floorMod(nextInt(), pInverse))
        v.foldRight(0)((x, sum) => x match {
            case 0 => sum + 1
            case _ => sum
        })
    }

    def main(args:Array[String]):Unit = {
        val n: Int = 600
        val delta: Double = 0.2
        val pInverse: Int = 6
        val averagingSamples: Int = 1000
        val mu = n.toDouble/pInverse
        val r = (1.0 - delta)*mu
        
        val res = (1 to averagingSamples).foldRight(0)((_, sum) => bernoulliX(n, pInverse) match {
            case x if (x.toDouble <= r) => sum + 1
            case _ => sum  
        }).toDouble/averagingSamples

        println(exp(-0.5*delta*delta*mu) + " >= " + res)
    }
}

object Task5 {

    def singleRun(s: Int, p: Double) = {
        val Yi = List.fill(s)(nextDouble())
        val mu = 0.5
    
        val Z = Yi.foldRight(0)((y, sum) => abs(y - mu) match {
            case z if (z <= p) => sum + 1
            case _ => sum
        })

        val pY = abs(Task4.median(Yi) - mu) match {
            case m if (m > p) => 1
            case _ => 0
        }

        val pZ = (s/2 - Z) match {
            case m  if (m > 0) => 1
            case _ => 0
        }
        (pY, pZ)
        
    }
    def main(args:Array[String]):Unit = {
        val s: Int = 600
        val p: Double = 1.0/6
        val n: Int = 1000
        val res = (1 to n).foldRight((0, 0))( (_, results) => {
            val t = singleRun(s, p)
            (results._1 + t._1, results._2 + t._2)
        })
        println(res._2.toDouble/n + " >= " + res._1.toDouble/n)
    }
}