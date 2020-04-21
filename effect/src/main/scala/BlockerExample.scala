package hello_cats.effect

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import cats.effect.{IO, Blocker, Sync}
import cats.effect.Sync
import cats.effect.ContextShift

object BlockerExample {

  val blockerContex =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)
  implicit val cs = IO.contextShift(ExecutionContext.global)
  implicit val blocker = Blocker.liftExecutionContext(blockerContex)

  def readLine[F[_]: Sync: ContextShift](
      implicit blocker: Blocker
  ): F[String] = {
    blocker.delay(
      scala.io.StdIn.readLine()
    )
  }

  def putStrLn[F[_]: Sync: ContextShift](
      s: String
  )(implicit blocker: Blocker): F[Unit] = {
    blocker.delay(
      println(s)
    )
  }

  def program: IO[Unit] = {
    for {
      in <- readLine[IO]
      _ <- putStrLn[IO](in)
    } yield ()
  }
}
