package hello_cats
package printable_lib

import org.scalatest.matchers.should._
import org.scalatest.freespec.AnyFreeSpec
import hello_cats.printable_lib.PrintableLib.Printable
import PrintableLib._
import PrintableLib.PrintableInstances._

class PrintableSpec extends AnyFreeSpec with Matchers {
  case class Cat(name: String, age: Int, color: String)

  object Cat {
    implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
      def format(c: Cat) = {
        val name  = Printable.format(c.name)
        val age   = Printable.format(c.age)
        val color = Printable.format(c.color)
        s"${name} is a ${age} year-old ${color} cat"
      }
    }
  }

  val cat = Cat("Kuzia", 9, "Gray")

  "Object intreface to Printable" in {
    Printable.format(cat) should be("Kuzia is a 9 year-old Gray cat")
  }
}
