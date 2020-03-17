package hello_cats
package printable_lib

import org.scalatest.matchers.should._
import org.scalatest.freespec.AnyFreeSpec
import hello_cats.printable_lib.PrintableLib.Printable
import PrintableLib._

class PrintableSpec extends AnyFreeSpec with Matchers {
  case class Cat(name: String, age: Int, color: String)

  object Cat {
    implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
      def format(c: Cat) = s"${c.name} is a ${c.age} year-old ${c.color} cat"
    }
  }

  val cat = Cat("Kuzia", 9, "Gray")

  "Object intreface to Printable" in {
    Printable.format(cat) should be("Kuzia is a 9 year-old Gray cat")
  }
}
