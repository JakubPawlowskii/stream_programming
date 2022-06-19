object DGIM {
  class dgim(val windowSize: Int, val r: Int) {

    var timestamp = 0
    var buckets: List[(Int, Int)] = List.empty[(Int, Int)] // (timestamp, size)
    
    private def maintainCondition(): Unit = {
        def rec(newBuckets: List[(Int, Int)]): List[(Int,Int)] = {
            // println(newBuckets)
            if (newBuckets.length <= r) newBuckets
            else{
                val cnt = newBuckets.head._2
                val group = newBuckets.takeWhile(b => b._2 == cnt)
                if (group.size <= r) group ::: rec(newBuckets.drop(group.size))
                else group.take(group.size - 2) ::: rec((group(group.size - 2)._1, group(group.size - 1)._2 + group(group.size - 1)._2) :: newBuckets.drop(group.size))
            } 
        }
        buckets = rec(buckets)
    }
    
    def parseStreamElement(element: Boolean): Unit = {
        
        try {
            if (buckets.last._1 == timestamp - windowSize)
                buckets =  buckets.dropRight(1)
            } catch {
                case e: NoSuchElementException => {}
            }
            
        if (element == false) {} // do nothing
        else {
            buckets = (timestamp, 1) :: buckets
            maintainCondition()
        }
        timestamp = timestamp + 1
        assert(buckets.groupBy(_._2).count(b => b._2.length > r) == 0)
    }

    // returns -1 if something is wrong
    def query(k: Int) = {
        val earliestBucket = buckets
        .findLast(b => b._1 <= timestamp && timestamp - k + 1 <= b._1)
        .getOrElse((-1, -1))
        if (earliestBucket == (-1, -1)) -1
        else{
            val lastIdx = buckets.indexOf(earliestBucket)
            buckets.take(lastIdx).foldLeft(0)((cnt, b) => cnt + b._2) + buckets(lastIdx)._2 / 2
      }
    }

  }

  def main(args: Array[String]): Unit = {

    val baseList = List(false, false, true)
    lazy val stream = LazyList.continually(baseList.to(LazyList)).flatten
    
    val bitCounter = new dgim(100,args(0).toInt)
    // stream.take(100).foreach(v => bitCounter.parseStreamElement(v))
    stream.take(1071).foreach(v => bitCounter.parseStreamElement(v))
    println("buckets = " + bitCounter.buckets)
    println("query = " + bitCounter.query(71))

    val tS = stream.take(1071).zipWithIndex.toList
    println("true value = " + tS.takeRight(71).count(p => p._1))

  }
}
/*
    Input: scala DGIM 2  
    Output:
        buckets = List((1070,1), (1067,2), (1061,2), (1055,4), (1043,4), (1031,8), (1007,16))
        query = 29
        true value = 24

    Input scala DGIM 10
    Output:
        buckets = List((1070,1), (1067,1), (1064,1), (1061,1), (1058,1), (1055,1), (1052,1),
        (1049,1), (1046,1), (1043,2), (1037,2), (1031,2), (1025,2), (1019,2), (1013,2),
        (1007,2), (1001,2), (995,2), (989,2), (983,4), (971,4))
        query = 24
        true value = 24
*/