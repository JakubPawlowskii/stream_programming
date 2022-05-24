import scala.language.implicitConversions
import scala.math.sqrt

package Complex {

  class Complex(r: Double, i: Double) {

    val real: Double = r
    val imag: Double = i

    def this(r: Double) =
      this(r, 0) // constructing real number as an instance of Complex class

    override def toString = {

      if (real.abs < 2 * Double.MinPositiveValue) { s"$imag" + s"i" }
      else if (imag.abs < 2 * Double.MinPositiveValue) { s"$real" }
      else {
        if (imag < 0.0) {
          s"$real - " + imag.abs.toString + s"i"
        } else {
          s"$real + $imag" + s"i"
        }
      }
    }

    def +(that: Complex): Complex = {
      new Complex(real + that.real, imag + that.imag)
    }
    def -(that: Complex): Complex = {
      new Complex(real - that.real, imag - that.imag)
    }
    def *(that: Complex): Complex = {
      new Complex(
        real * that.real - imag * that.imag,
        real * that.imag + imag * that.real
      )
    }
    def inverse = {
      val denom = real * real + imag * imag
      require(denom != 0)
      new Complex(real / denom, -imag / denom)
    }
    def /(that: Complex): Complex = this * that.inverse
    def unary_- = new Complex(-real, -imag)
    def conj: Complex = new Complex(real, -imag)
    lazy val modulus = sqrt(real * real + imag * imag)
  }

  object Complex {
    def apply(real: Double, imag: Double) = new Complex(real, imag)
    def apply(real: Double) = new Complex(real)
    def apply(real: Int, imag: Int) = new Complex(real.toDouble, imag.toDouble)
    def apply(real: Int) = new Complex(real.toDouble)
    implicit def DoubleToComplex(r: Double) = new Complex(r)
    implicit def IntToComplex(r: Int) = new Complex(r.toDouble)

  }
}
