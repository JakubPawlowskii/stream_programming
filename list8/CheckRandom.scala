import HyperLogLog.HyperLogLog.intCounter
import scala.util.Random
import scala.math.pow

object CheckRandom {
  def main(args: Array[String]): Unit = {

    val numberOfRuns = 10
    val p = args(0).toInt
    val b = args(1).toInt

    val counter = intCounter(b, true)
    val k = pow(2, p).intValue

    val runs = List.fill(numberOfRuns)(0.0).map { n =>
      {
        var K = k
        while (K > 0) {
          counter.add(Random.nextInt())
          K = K - 1
        }
        val tmp = counter.getCount()
        counter.reset()
        tmp
      }
    }

    val avgEstimate = pow(2, 32) - (runs.sum / numberOfRuns)
    val analytical = pow(2, 32) * pow((1.0 - 1.0 / pow(2, 32)), pow(2, p))
    println("Number of integers that have never been generated:")
    println("Estimate: " + avgEstimate.toString + " --- " + (avgEstimate / pow(2,32) * 100).toString + "%")
    println("Analytical: " + analytical.toString + " --- " + (analytical / pow(2,32) * 100).toString + "%")
  }

}

/*
  input: scala CheckRandom 28 10   <-- 2^10 is the number of buckets
  output:
    Number of integers that have never been generated:
    Estimate: 4.0172604513591957E9 --- 93.53413366151963%
    Analytical: 4.0347483821897154E9 --- 93.94130628066407%
  
  input: scala CheckRandom 29 10
  output:
    Number of integers that have never been generated:
    Estimate: 3.7146338381040044E9 --- 86.48805874641995%
    Analytical: 3.790295335366979E9 --- 88.24969025717535%

  input: scala CheckRandom 30 10
  output: 
    Number of integers that have never been generated:
    Estimate: 3.0941800082100863E9 --- 72.041992289249%
    Analytical: 3.3449238932935243E9 --- 77.88007830487388%
*/
