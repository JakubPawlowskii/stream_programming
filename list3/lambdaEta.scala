object lambdaEta {

  def fM(x: String): String = "f(" + x + ")"
  def gM(x: String): String = "g(" + x + ")"

  val fF: String => String = (x: String) => "f(" + x + ")"
  val gF: String => String = (x: String) => "g(" + x + ")"

  def gM2(x: String, leftPar: Char, rightPar: Char): String =
    "g" + leftPar + x + rightPar

  def main(args: Array[String]): Unit = {

    val F = (fF andThen gF)("x")
    // val M = (fM andThen gM)("x")  does not work
    val M = (fM _ andThen gM _)("x") // converted method to λ-function in place via η-expansion
    println(F)
    println(M)

    // val test1 = (fM _ andThen gM2(_: String, '[', ']'))("x")
    val test2 = (fM _ andThen(gM2(_: String, '[', ']')))("x")

    // println(test1)
    println(test2)
    /*
            First expression does not work because without () gM2(_: String, '[', ']') is
            treated like a its return type, which is String, whereas in the second expression
            it is treated like a map String=>String, which can then be composed with fM _
            via andThen.
            Perhaps it has something to do with the order of evaluation, i.e. without () addThen tries
            to use gM2 before it is converted to partially applied function and sees only the String
            return type. However, evaluation of the following:
              (fM _ andThen gM2(_, '[',']') )("x")
            yields an error, because Scala cannot infer the type of placeholder argument in gM2, which
            can suggest that it is treated like a partially applied function.
     */
  }
}
