object listLength {

    def lengthRight[A](as: List[A]): Int = {
        as.foldRight(0)((x,y) => y+1)
    }
  
    def main(args: Array[String]): Unit = {

        val a = List(1,2,3,4,5,6,7,8)
        val b = List("a","b","c","d")
        val c = List.tabulate(10)(n => math.sqrt(n))
        val d = List.range(1,100,2)
        val e = List(1 to 1000: _*)
        
        println(a.length == lengthRight(a))
        println(b.length == lengthRight(b))
        println(c.length == lengthRight(c))
        println(d.length == lengthRight(d))
        println(e.length == lengthRight(e))

  }
}