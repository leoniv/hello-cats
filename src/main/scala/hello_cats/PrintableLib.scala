package hello_cats
package printable_lib

object PrintableLib {
  trait Printable[A] {
    def format(a: A): String
  }

  object Printable {
    def format[A](v: A)(implicit instance: Printable[A]) = instance.format(v)

    def print[A](v: A)(implicit instance: Printable[A]): Unit = {
      println(format(v))
    }
  }

  object PrintableInstances {
    implicit val intPrintable: Printable[Int] = new Printable[Int] {
      def format(i: Int) = i.toString
    }

    implicit val stringPrintable: Printable[String] = new Printable[String] {
      def format(a: String): String = a
    }
  }
}

