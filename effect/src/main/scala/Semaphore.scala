package hello_cats.effect

import cats.effect._
import cats.effect.concurrent._
import scala.concurrent.ExecutionContext
import cats.implicits._
import scala.concurrent.duration._

object SemaphoreExample {
  implicit val ec = ExecutionContext.global
  implicit val cs = IO.contextShift(ec)
  implicit val timer = IO.timer(ec)

  def resourse[F[_]: Concurrent](number: Int, s: Semaphore[F])(implicit timer: Timer[F]) = {
    def putStrLn(mess: String): F[Unit] = Sync[F].delay(println(mess))

    for {
      x <- s.available
      _ <- putStrLn(show"#$number >> availablity: $x")
      _ <- s.acquire
      x <- s.available
      _ <- putStrLn(show"#$number >> Start | availablity: $x")
      _ <- timer.sleep(1.second)
      _ <- s.release
      x <- s.available
      _ <- putStrLn(show"#$number >> Done | availablity: $x")
    } yield ()
  }

  val program = for {
    s <- Semaphore[IO](2)
    s <- (1 until 6).toList.map(resourse(_, s)).parSequence.void
  } yield ()

}
