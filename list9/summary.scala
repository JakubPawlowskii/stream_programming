import MisraGries.MisraGries.misraGries
import scala.io._
object summary {

  // This is copied from https://stackoverflow.com/a/55143951/18453675
  def formatTable(table: Seq[Seq[Any]]): String = {
    if (table.isEmpty) ""
    else {
      // Get column widths based on the maximum cell width in each column (+2 for a one character padding on each side)
      val colWidths = table.transpose.map(
        _.map(cell => if (cell == null) 0 else cell.toString.length).max + 2
      )
      // Format each row
      val rows = table.map(
        _.zip(colWidths)
          .map { case (item, size) => (" %" + (size - 1) + "s").format(item) }
          .mkString("|", "|", "|")
      )
      // Formatted separator row, used to separate the header and draw table borders
      val separator = colWidths.map("-" * _).mkString("+", "+", "+")
      // Put the table together and return
      (separator +: rows.head +: separator +: rows.tail :+ separator)
        .mkString("\n")
    }
  }

  def main(args: Array[String]): Unit = {

    val file = args(0)
    val k = args(1).toInt
    val elim = args(2).toInt
    val source = Source.fromFile(file)
    val words = source.mkString.split("\\W+").map(_.toLowerCase)
    source.close

    val threshold = words.length / k.toDouble
    println("Threshold frequency is " + threshold)

    val exactFrequencies = words
      .groupBy(identity)
      .toList
      .map(n => (n._1, n._2.size))
      .toMap

    val exactWordsOverThreshold = exactFrequencies.toList
      .filter(v => v._2 > threshold)
      .sortBy(_._2)
      .reverse
      .toMap

    val mg = misraGries(k)
    mg.summarizeStream(words)
    if (elim == 1) {
      mg.eliminateFalsePositive(words)
      println("False positives eliminated.")
    }
    
    val mgWordsOverThreshold = mg.getSummary()
    val check = exactWordsOverThreshold.filter(s => mgWordsOverThreshold.contains(s._1)).size == exactWordsOverThreshold.size
    if(check)
    {
      println("All words with frequencies larger than threshold were found by Misra-Gries summary.")
    }
    else{
      println("Not all words with frequencies larger than threshold were found by Misra-Gries summary.")
      println("Words missed : ")
      exactWordsOverThreshold.filter(s => !mgWordsOverThreshold.contains(s._1)) foreach println
    }
    
    val headers = Seq(
      "Word",
      "Exact frequency",
      "Lower bound",
      "MG frequency",
      "False positive",
      "Theoretical estimate satisfied"
    )
    val w = mgWordsOverThreshold.map(v => v._1).toSeq
    val exact = mgWordsOverThreshold.map(p => exactFrequencies(p._1)).toSeq
    val lowerBound = mgWordsOverThreshold
      .map(p => exactFrequencies(p._1) - threshold)
      .toSeq
      .map(v => {
        if (v > 0) { v.round }
        else { 0 }
      })
    val mgFreq = mgWordsOverThreshold.map(p => p._2).toSeq
    val FalsePositive = mgWordsOverThreshold
      // .map(p => exactFrequencies(p._1) < threshold)
      .map(p => mgWordsOverThreshold(p._1) < threshold)
      .toSeq
      .map(v => {
        if (v) { "Yes" }
        else { "No" }
      })
    val TheoreticalEstimate = Seq
      .tabulate(mgWordsOverThreshold.size)(i =>
        lowerBound(i) < mgFreq(i) && mgFreq(i) < exact(i)
      )
      .toSeq
      .map(v => {
        if (v) { "Yes" }
        else { "No" }
      })

    val tmp =
      Seq(w, exact, lowerBound, mgFreq, FalsePositive, TheoreticalEstimate)
   
    val vals = tmp
      .map(v =>
        v.sortWith((left, right) =>
          exact(v.indexOf(left)) > exact(v.indexOf(right))
        )
      )
      .transpose

    val table = formatTable(headers +: vals)
    println(table)

  }
}

/*
  Input: scala summary canterbury-corpus/canterbury/alice29.txt 21 0  (no correction of false positives)
  Output: 
        Threshold frequency is 1301.6190476190477
        All words with frequencies larger than threshold were found by Misra-Gries summary.
        +------+-----------------+-------------+--------------+----------------+--------------------------------+
        | Word | Exact frequency | Lower bound | MG frequency | False positive | Theoretical estimate satisfied |
        +------+-----------------+-------------+--------------+----------------+--------------------------------+
        | the  | 1642            | 340         | 383          | No             | Yes                            |
        | and  | 872             | 0           | 5            | Yes            | Yes                            |
        | of   | 513             | 0           | 3            | Yes            | Yes                            |
        +------+-----------------+-------------+--------------+----------------+--------------------------------+


Input: scala summary canterbury-corpus/canterbury/alice29.txt 21 1   (removing false positives with a second pass)                                                           
Output:
        Threshold frequency is 1301.6190476190477
        False positives eliminated.
        All words with frequencies larger than threshold were found by Misra-Gries summary.
        +------+-----------------+-------------+--------------+----------------+--------------------------------+
        | Word | Exact frequency | Lower bound | MG frequency | False positive | Theoretical estimate satisfied |
        +------+-----------------+-------------+--------------+----------------+--------------------------------+
        | the  | 1642            | 340         | 383          | No             | Yes                            |
        +------+-----------------+-------------+--------------+----------------+--------------------------------+
        
Input: scala summary canterbury-corpus/large/bible.txt 21 0                                                                               
Output:
    Threshold frequency is 36564.52380952381
    All words with frequencies larger than threshold were found by Misra-Gries summary.
    +-------+-----------------+-------------+--------------+----------------+--------------------------------+
    | Word  | Exact frequency | Lower bound | MG frequency | False positive | Theoretical estimate satisfied |
    +-------+-----------------+-------------+--------------+----------------+--------------------------------+
    | the   | 61680           | 25115       | 27142        | No             | Yes                            |
    | and   | 49862           | 13297       | 15324        | No             | Yes                            |
    | of    | 33195           | 0           | 85           | Yes            | Yes                            |
    | lord  | 7670            | 0           | 1            | Yes            | Yes                            |
    | all   | 5430            | 0           | 1            | Yes            | Yes                            |
    | you   | 2590            | 0           | 1            | Yes            | Yes                            |
    | come  | 1919            | 0           | 1            | Yes            | Yes                            |
    | jesus | 983             | 0           | 1            | Yes            | Yes                            |
    | amen  | 78              | 0           | 1            | Yes            | Yes                            |
    +-------+-----------------+-------------+--------------+----------------+--------------------------------+


Input:  scala summary canterbury-corpus/large/bible.txt 21 1                                                                                
Output: 
        Threshold frequency is 36564.52380952381
        False positives eliminated.
        All words with frequencies larger than threshold were found by Misra-Gries summary.
        +------+-----------------+-------------+--------------+----------------+--------------------------------+
        | Word | Exact frequency | Lower bound | MG frequency | False positive | Theoretical estimate satisfied |
        +------+-----------------+-------------+--------------+----------------+--------------------------------+
        | the  | 61680           | 25115       | 27142        | No             | Yes                            |
        | and  | 49862           | 13297       | 15324        | No             | Yes                            |
        +------+-----------------+-------------+--------------+----------------+--------------------------------+

*/