object loopLess {
  def main(args: Array[String]): Unit = {

    val range = 2 to 100
    val res1Loop =
      for (i <- range if i % 2 != 0; if i % 3 != 0; if i % 5 != 0) yield (i * i)
    val res1LoopLess = range
      .withFilter(x => x % 2 != 0 && x % 3 != 0 && x % 5 != 0)
      .map(x => x * x)

    // res1Loop zip res1LoopLess foreach { println }
    /*
            (49,49)
            (121,121)
            (169,169)
            (289,289)
            (361,361)
            (529,529)
            (841,841)
            (961,961)
            (1369,1369)
            (1681,1681)
            (1849,1849)
                    .
                    .
                    .
     */
    println(res1Loop.zip(res1LoopLess).map(x => x._1 - x._2).sum) // 0

    val res2Loop =
      for (i <- range if i % 2 != 0; j <- range if j % 2 == 0) yield (i, j)
    val res2LoopLess = range.flatMap(x =>
      range.withFilter(y => y % 2 == 0 && x % 2 != 0).map(y => (x, y))
    )

    // res2Loop zip res2LoopLess foreach {println} // too long output to paste here
    val sum = res2Loop
      .zip(res2LoopLess)
      .map(x => (x._1._1 - x._2._1, x._1._2 - x._2._2))
      .map(x => x._1 + x._2)
      .sum
    println(sum) // 0
  }
}
