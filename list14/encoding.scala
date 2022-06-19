object encoding{
    import polynomialGF2.polynomialGF2._
      def toBinary(i: BigInt, digits: Int) = String.format("%" + digits + "s", i.toString(2)).replace(' ', '0')
      
    def main(args: Array[String]): Unit = {

        val g = polynomialGF2("1010100111001")
        println("Generator polynomial g(x) = " + g)

        val msg = "000000000000000000000000000000000000000000000000111"
        println("Message is " + msg)
        println("Message length is " + msg.length)
        val polyMsg = polynomialGF2(msg)
        println("Message polynomial is " + polyMsg)
        val encodedMsg = polyMsg *** g
        println("Encoded message is " + toBinary(encodedMsg.rep,63))
        println("Encoded message length is " + toBinary(encodedMsg.rep,63).length)
        println("Encoded message polynomial is " + encodedMsg)
    }
}

/*
  Generator polynomial g(x) = x^12 + x^10 + x^8 + x^5 + x^4 + x^3 + 1
  Message is 000000000000000000000000000000000000000000000000111
  Message length is 51
  Message polynomial is x^2 + x + 1
  Encoded message is 000000000000000000000000000000000000000000000000110101110101111
  Encoded message length is 63
  Encoded message polynomial is x^14 + x^13 + x^11 + x^9 + x^8 + x^7 + x^5 + x^3 + x^2 + x + 1
*/