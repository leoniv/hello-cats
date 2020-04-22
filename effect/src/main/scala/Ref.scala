package hello_cats.effect

import cats.effect._
import cats.effect.concurrent._
import scala.concurrent.ExecutionContext
import cats.implicits._

object RefExample {
  implicit val ec = ExecutionContext.global
  implicit val cs = IO.contextShift(ec)

  def worker[F[_]: Sync](number: Int, counter: Ref[F, Int]) = {
    def putStrLn(mess: String): F[Unit] = Sync[F].delay(println(mess))

    for {
      count <- counter.get
      _ <- putStrLn(show"#$number >> ${count}")
      count <- counter.modify(i => (i + 1, i))
      _ <- putStrLn(show"#$number >> ${count}")
    } yield ()
  }

  val program = for {
    ref <- Ref.of[IO, Int](0)
    _ <- (1 until 4).toList.map(i => worker(i, ref)).parSequence.void
    count <- ref.get
  } yield count

}
