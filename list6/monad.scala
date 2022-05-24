object monad {

  val f = (i: Int) => Option(i * i)
  val g = (i: Int) => Option(i * i * i)

  def unit[T](x: T): Option[T] = Some(x)

  def main(args: Array[String]): Unit = {

    val m1 = Some(2)
    val m2 = None
    val x = 3

    // Associativity
    val left1 = (m1 flatMap f ) flatMap g 
    val right1 = m1 flatMap (x => f(x) flatMap g)
    val left2 = (m2 flatMap f ) flatMap g 
    val right2 = m2 flatMap (x => f(x) flatMap g)
    println(left1 == right1)  // true
    println(left2 == right2)  // true

    // Left unit 
    val leftUnitLeft = unit(x) flatMap f
    val leftUnitRight = f(x)
    println(leftUnitLeft == leftUnitRight)  // true

    // Right unit

    val rightUnitLeft1 = m1 flatMap (y => unit(y))
    println(rightUnitLeft1 == m1)   // true
    val rightUnitLeft2 = m2 flatMap (y => unit(y))
    println(rightUnitLeft2 == m2)   // true

  }
}
