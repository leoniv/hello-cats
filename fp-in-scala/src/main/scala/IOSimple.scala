package fp.in.scala

import scala.io.StdIn

object IOSimple {
  sealed trait IO[A] { self =>
    def run: A
    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
  }

  object IO extends Monad[IO] {
    def apply[A](a: => A): IO[A] = unit(a)
    def ret[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A, B](m: IO[A])(f: A => IO[B]): IO[B] = m flatMap f
    def tailRecM[A, B](a: A)(f: A => IO[Either[A,B]]): IO[B] = defaultTailRecM(a)(f)
  }

  def PrintLine(s: String): IO[Unit] = IO { println(s) }
  def ReadLine(): IO[String] = IO { "input" }

  def program: IO[Unit] =
    for {
      s <- ReadLine.map(s => s"Mapped $s")
      s <- PrintLine(s"Echo: ")
    } yield ()

  def programStackOverflow = IO.forever(PrintLine("Still going..."))
}
