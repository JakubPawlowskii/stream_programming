object polynomialsGF2 {

  class polynomialGF2(coeffs: String) {

    require(coeffs.length <= 32)

    def this(intRep: Int) = this(intRep.toBinaryString)

    val rep = Integer.parseInt(coeffs, 2)
    lazy val listOfCoeffs = coeffs.reverse.split("").map(s => s.toInt)

    override def toString = {

      var cnt = listOfCoeffs.length
      var len = listOfCoeffs.count(c => c == 1)
      val str = listOfCoeffs.foldRight("")((c, s) => {
        if (c == 1) {
          cnt = cnt - 1
          len = len - 1
          if (cnt == 0) 
            {
                s + "1"
            } 
          else if (cnt == 1) 
            {
                if (len == 0) s + "x"
                else s + "x + "
            }
            else
            {
                if(len == 0) s + "x^" + cnt.toString
                else s + "x^" + cnt.toString + " + "
            } 
        } else {
          cnt = cnt - 1
          s
        }
      })
      str
    }

    def +(that: polynomialGF2) = new polynomialGF2(rep ^ that.rep)
    def *(that: polynomialGF2) = {
        var res = 0
        var thisRep = rep
        var thatRep = that.rep

        while(thisRep != 0)
        {
            if ((thisRep & 1) == 1) res ^= thatRep
            thisRep = thisRep >> 1
            thatRep = thatRep << 1
        }
        new polynomialGF2(res)
    }

  }

  def main(args: Array[String]): Unit = {

    val p1 = new polynomialGF2("1011")
    val p2 = new polynomialGF2("10111")
    // val p1 = new polynomialGF2(11)
    // val p2 = new polynomialGF2(23)
    println("p1 = " + p1)
    println("p2 = " + p2)
    println("p1 + p2 = " + (p1 + p2))
    println("p1 + p2 as integer is " + (p1 + p2).rep.toString)
    println("p1 * p2 = " + (p1 * p2))
    println("p1 * p2 as integer is " + (p1 * p2).rep.toString)
  }
}
/*
Output:
    p1 = x^3 + x + 1
    p2 = x^4 + x^2 + x + 1
    p1 + p2 = x^4 + x^3 + x^2
    p1 + p2 as integer is 28
    p1 * p2 = x^7 + 1
    p1 * p2 as integer is 129
*/