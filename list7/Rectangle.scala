import Element.elem
import scala.util.{Try, Success, Failure}
import scala.io._
object Rectangle {

  val horizontalLine = '='
  val verticalLine   = '|'
  val corner         = elem("*")

  def rectangle(h: Int, w: Int): Element = {
    require(h >= 2 && w >= 2)
    if (h == 2 && w > 2) {
      val horizontalElem =
        corner beside elem(horizontalLine, w - 2, 1) beside corner
      horizontalElem above horizontalElem

    } else if (h > 2 && w == 2) {

      val horizontalElem = corner beside corner
      val verticalElem = elem(verticalLine, 1, h - 2) beside elem(' ', w - 2, h - 2) beside elem(
        verticalLine,
        1,
        h - 2
      )
      horizontalElem above verticalElem above horizontalElem
    } else if (h == 2 && w == 2) {
      (corner beside corner) above (corner beside corner)
    } else {

      val horizontalElem =
        corner beside elem(horizontalLine, w - 2, 1) beside corner
      val verticalElem =
        elem(verticalLine, 1, h - 2) beside elem(' ', w - 2, h - 2) beside elem(
          verticalLine,
          1,
          h - 2
        )
      horizontalElem above verticalElem above horizontalElem
    }
  }

  def main(args: Array[String]) = {

    val (w, h) = Try((args(0).toInt, args(1).toInt)) match {
      case Success((w, h)) => (w, h)
      case Failure(e) => {
        println(e)
        println("Constructing 3x3 rectangle")
        (3, 3)
      }
    }

    println(rectangle(h, w))

    /*
      Example output:
        scala Rectangle 5 4
        *===*
        |   |
        |   |
        *===*
    */
  }
}
