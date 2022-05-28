
package polynomialGF2
{
  import  java.lang.Long.parseLong
  import  java.lang.Long.numberOfLeadingZeros
  import  scala.annotation.tailrec

  object polynomialGF2 {
    def polynomialGF2(coeffs: String) = new polynomialGF2(coeffs)
    def polynomialGF2(rep: Long) = new polynomialGF2(rep)
  }

  class polynomialGF2(coeffs: String) {
    
    require(coeffs.length <= 64)
    def this(intRep: Long) = this(intRep.toBinaryString)
    
    val rep = parseLong(coeffs,2)
    val degree = coeffs.length - 1
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
        assert(that.degree * this.degree <= 64)
        var res = 0L
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
      def %(divisor: polynomialGF2) = {

        assert(divisor.rep != 0)
        if(divisor.degree > this.degree) (new polynomialGF2(0), this)
        else{      
          
          val deg_den = divisor.degree
          val den = divisor.rep
          
          @tailrec
          def division(n: Long, q:Long): (polynomialGF2,polynomialGF2) = {
            val deg_num = 63 - numberOfLeadingZeros(n)
            if(deg_den > deg_num) (new polynomialGF2(q), new polynomialGF2(n))
            else division(n ^ (den << (deg_num - deg_den)), q ^ (1 << (deg_num - deg_den)))
          }
          
          division(this.rep, 0)
        }
      }

      def reciprocal(): polynomialGF2 = new polynomialGF2(this.coeffs.reverse)
    }
    
    
  }
  