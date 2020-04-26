package fp.in.scala

import scala.io.StdIn

object IOSimple {
  sealed trait Monad[F[_]] {
    def unit[A](a: A): F[A]
    def map[A, B](m: F[A])(f: A => B): F[B]
    def flatMap[A, B](m: F[A])(f: A => F[B]): F[B]
  }

  sealed trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      def run = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    def unit[A](a: A): IO[A] = new IO[A] { def run = a }
    def map[A, B](m: IO[A])(f: A => B): IO[B] = m map f
    def flatMap[A, B](m: IO[A])(f: A => IO[B]): IO[B] = m flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
  }

  def PrintLine(s: String): IO[Unit] = IO { println(s) }
  def ReadLine(): IO[String] = IO { "input" }

  def program: IO[Unit] =
    for {
      s <- ReadLine.map(s => s"Mapped $s")
      s <- PrintLine(s"Echo: $s")
    } yield ()
}
