package polynomialGF2 {

  import scala.annotation.tailrec

  object polynomialGF2 {
    def polynomialGF2(coeffs: String) = new polynomialGF2(coeffs)
    def polynomialGF2(rep: BigInt) = {
      assert(rep >= 0)
      new polynomialGF2(rep)
    }
  }

  class polynomialGF2(val rep: BigInt) {

    def this(coeffs: String) = this(BigInt(coeffs,2))

    val coeffs = rep.toString(2)
    val degree = rep.bitLength - 1
    lazy val listOfCoeffs = coeffs.reverse.split("").map(s => s.toInt)

    override def toString = {

      var cnt = listOfCoeffs.length
      var len = listOfCoeffs.count(c => c == 1)
      val str = listOfCoeffs.foldRight("")((c, s) => {
        if (c == 1) {
          cnt = cnt - 1
          len = len - 1
          if (cnt == 0) {
            s + "1"
          } else if (cnt == 1) {
            if (len == 0) s + "x"
            else s + "x + "
          } else {
            if (len == 0) s + "x^" + cnt.toString
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

    def **(that: polynomialGF2) = {

      def mult(a: BigInt, b: BigInt): BigInt = {  
        if (a * b == 0) 0
        else if (a == 1) b
        else if (b == 1) a
        else (mult(a >> 1, b) << 1) ^ (a % 2 * b)
      }
      new polynomialGF2(mult(this.rep, that.rep))
    }

    def ***(that: polynomialGF2) = {
      if(this.rep * that.rep == 0) new polynomialGF2(0)
      else if (this.rep == 1) that
      else if (that.rep == 1) this
      else{
        @tailrec
        def mult(a: BigInt, b: BigInt, res: BigInt): BigInt = {
            if (a == 0) res
            else mult(a.flipBit(a.lowestSetBit), b, res ^ (b << a.lowestSetBit))
        }
        new polynomialGF2( mult(this.rep, that.rep, BigInt(0)) )
      }
    }

    def *(that: polynomialGF2) = {
      var res = BigInt(0)
      var thisRep = rep
      var thatRep = that.rep

      while (thisRep != 0) {
        if ((thisRep & 1) == 1) res ^= thatRep
        thisRep = thisRep >> 1
        thatRep = thatRep << 1
      }
      new polynomialGF2(res)
    }
    def %(divisor: polynomialGF2) = {

      assert(divisor.rep != 0)
      if (divisor.degree > this.degree) (new polynomialGF2(0), this)
      else {

        val deg_den = divisor.degree
        val den = divisor.rep
        @tailrec
        def division(n: BigInt, q: BigInt): (polynomialGF2, polynomialGF2) = {

          val deg_num = n.bitLength - 1
          if (deg_den > deg_num) (new polynomialGF2(q), new polynomialGF2(n))
          else
            division(
              n ^ (den << (deg_num - deg_den)),
              q ^ (BigInt(1) << (deg_num - deg_den))
            )
        }

        division(this.rep, BigInt(0))
      }
    }

    def gcd(that: polynomialGF2): polynomialGF2 = {

      @tailrec
      def euclid(a: polynomialGF2, b: polynomialGF2): polynomialGF2 = {
        if (b.rep == BigInt(0)) a
        else euclid(b, (a % b)._2)
      }
      euclid(this, that)
    }

    def reciprocal(): polynomialGF2 = new polynomialGF2(this.coeffs.reverse)
    def lcm(that: polynomialGF2): polynomialGF2 =
      ((this * that) % (gcd(that)))._1
  }

}
