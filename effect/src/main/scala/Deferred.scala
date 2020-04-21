package hello_cats.effect

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Deferred
import scala.concurrent.ExecutionContext

object DeferredExample {
  implicit val cs = IO.contextShift(ExecutionContext.global)

  def complete(d: Deferred[IO, Int]): IO[Unit] = {
    val concurrentComl = (n: Int) => d.complete(n).attempt.void

    List(
      IO.race(concurrentComl(1), concurrentComl(2)),
      d.get.flatMap { i => IO { println(show"Win $i") } }
      ).parSequence.void
  }

  def program = for {
    d <- Deferred[IO, Int]
    _ <- complete(d)
  } yield ()

}
