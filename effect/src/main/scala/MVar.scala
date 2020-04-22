package hello_cats.effect

import cats.implicits._
import cats.effect._
import cats.effect.concurrent._
import scala.concurrent.ExecutionContext
import cats.Traverse

object MVarExample {
  type Chanel[A] = MVar[IO, A]

  implicit val ec = ExecutionContext.global
  implicit val cs = IO.contextShift(ec)

  def producer[F[_]: Traverse](
      ch: Chanel[Option[Int]],
      numbers: F[Int]
  ): IO[Unit] =
    for {
      _ <- Traverse[F].traverse(numbers)(i => ch.put(Some(i)))
      _ <- ch.put(None)
    } yield ()

  def consumer(ch: Chanel[Option[Int]], sum: Long): IO[Long] =
    for {
      n <- ch.take
      sum <- n match {
        case None    => IO(sum)
        case Some(n) => consumer(ch, sum + n)
      }
    } yield sum

  def program =
    for {
      ch <- MVar[IO].empty[Option[Int]]
      fp <- producer(ch, (0 until 100).toList).start
      fc <- consumer(ch, 0L).start
      _ <- fp.join
      sum <- fc.join
    } yield sum
}
