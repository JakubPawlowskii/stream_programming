import scala.util.Random._
import scala.math.abs

object sumAndMedian {

  def median(seq: Seq[Double]): Double = {
    val sortedSeq = seq.sorted
    if (seq.size % 2 == 1) sortedSeq(sortedSeq.size / 2)
    else {
      val (up, down) = sortedSeq.splitAt(seq.size / 2)
      (up.last + down.head) / 2
    }
  }

  def singleRun(s: Int): (Int, Int) = {
    
    val Yi = List.fill(s)(nextDouble())

    // val muExact = Yi.sum / Yi.length
    val muExact = 1.0 / 2.0
    // println("Exact μ = " + muExact.toString)
    // println("Approximate μ = " + muApprox.toString)

    val threshold = 1.0 / 6.0
    // val pr = Yi.count(x => abs(x - muExact) <= threshold).toDouble / Yi.length
    // println("Exact P(|Yi - μ| ≤ 1/6) = 1/3")
    // println("Approximated P(|Yi - μ| ≤ 1/6) ≈ " + pr.toString)

    val med = median(Yi)
    val LHS = abs(med - muExact) match {
      case s if (s > threshold) => 1
      case _ => 0
    }

    val Z = Yi
      .map(x => {
        if (abs(x - muExact) > threshold) 0
        else 1
      })
      .sum

    val RHS = (Z - s/2.0) match {
      case s if (s < 0) => 1
      case _ => 0
    }
    (LHS, RHS)
  }

  def main(args: Array[String]): Unit = {

    val s = 600
    val rep = 1000

    val r = List.fill(rep)(singleRun(s))
   
    val LHS = r.map(x => x._1).sum.toDouble / rep
    val RHS = r.map(x => x._2).sum.toDouble / rep

    println("LHS = " + LHS.toString)
    println("RHS = " + RHS.toString)
    println("Is LHS ≤ RHS?  " + (LHS <= RHS).toString)

  }
}

/*
  Output:
    LHS = 0.0
    RHS = 1.0
    Is LHS ≤ RHS?  true
*/