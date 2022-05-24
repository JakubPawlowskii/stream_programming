object mapNew {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case _         => Some(f(a.get, b.get))
    }
  }

  def add(x: Int, y: Double): String = { (x.toDouble + y).toString }
  def main(args: Array[String]): Unit = {

    val x = 5
    val y = 4.0
    println(add(x, y))
    println(map2(Some(x), Some(y))(add))
    println(map2(Some(x), None)(add))
    println(map2(None, Some(y))(add))
    println(map2(None, None)(add))

  }

  /*
        Output:
            9.0
            Some(9.0)
            None
            None
            None
   */
}
