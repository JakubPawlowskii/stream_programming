object polyDivision{
    import scala.math.pow
    import polynomialGF2.polynomialGF2._
    def main(args: Array[String]): Unit = {
        println("Assignment 1")        
        val p1 = polynomialGF2("10000001")
        val p2 = polynomialGF2("101")
        println("p1 = " + p1)
        println("p2 = " + p2)
        
        val (q,r) = p1 % p2
        println("p1 = q*p2 + r")
        println("(" + p1.toString + ") = (" + q.toString + ") * (" + p2.toString + ") + (" + r.toString + ")")
        println("q in decimal representation : " + q.rep)
        println("r in decimal representation : " + r.rep)

        println("==========================================")
        println("Assignment 2")
        val p3 = polynomialGF2(pow(2,15).toInt + 1)
        val p4 = polynomialGF2("10000011")
        println(p3)
        println(p4)
        val (q1,r1) = p3 % p4
        println(p3.toString + " : " + p4.toString + " = (" + q1.toString + " , " + r1.toString + ")") 
        println("q in decimal representation : " + q1.rep)
        println("r in decimal representation : " + r1.rep)
        println("==========================================")
        println("Assignment 3")

        val bch = polynomialGF2("1010100111001")
        val bchReciprocal = bch.reciprocal()
        val tmp = polynomialGF2(BigInt(2).pow(63) + 1)
        val bchDual = tmp % bchReciprocal
        
        println("bch = " + bch)
        println("bch reciprocal = " + bchReciprocal)
        println("bch dual = " + bchDual._1)
        println("bch dual in decimal representation = " + bchDual._1.rep)

        println("==========================================")
        println("Assignment 5")

        val m1 = polynomialGF2("10001001")
        val m2 = polynomialGF2("10001111")

        val g = m1.lcm(m2)
        println("m1 = " + m1.toString)
        println("m2 = " + m2.toString)
        println("g = lcm(m1,m2) =  " + g)
        println("g in decimal representation = " + g.rep)
    }
}

/*
Assignment 1
p1 = x^7 + 1
p2 = x^2 + 1
p1 = q*p2 + r
(x^7 + 1) = (x^5 + x^3 + x) * (x^2 + 1) + (x + 1)
q in decimal representation : 42
r in decimal representation : 3
==========================================
Assignment 2
x^15 + 1
x^7 + x + 1
x^15 + 1 : x^7 + x + 1 = (x^8 + x^2 + x , x^3 + x + 1)
q in decimal representation : 262
r in decimal representation : 11
==========================================
Assignment 3
bch = x^12 + x^10 + x^8 + x^5 + x^4 + x^3 + 1
bch reciprocal = x^12 + x^9 + x^8 + x^7 + x^4 + x^2 + 1
bch dual = x^51 + x^48 + x^47 + x^46 + x^45 + x^42 + x^41 + x^39 + x^35 + x^34 + x^30 + x^29 + x^27 + x^26 + x^25 + x^24 + x^23 + x^22 + x^19 + x^16 + x^13 + x^12 + x^11 + x^9 + x^7 + x^6 + x^2 + 1
bch dual in decimal representation = 2786765635664581
==========================================
Assignment 5
m1 = x^7 + x^3 + 1
m2 = x^7 + x^3 + x^2 + x + 1
g = lcm(m1,m2) =  x^14 + x^9 + x^8 + x^6 + x^5 + x^4 + x^2 + x + 1
g in decimal representation = 17271
*/