package hello_cats.effect

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import cats.effect.{IO, Blocker, Sync, Fiber}
import cats.effect.Sync
import cats.effect.ContextShift
import scala.concurrent.duration._
import cats.effect.IOApp
import cats.effect.ExitCode

object BlockerExample {
  class App extends IOApp {
    def run(args: List[String]): IO[ExitCode] = {
      IO(ExitCode.Success)
    }

    def blockerInst = blocker
  }

  def ioApp = new App

  val exeContex =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)
  implicit val cs = IO.contextShift(ExecutionContext.global)
  val blocker = Blocker.liftExecutionContext(exeContex)

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

  def infiniteIO(id: Int)(blocker: Blocker): IO[Fiber[IO, Unit]] = {
    def repeat: IO[Unit] =
      blocker.delay[IO, Unit](println(id)).flatMap(_ => repeat)
    repeat.start(IO.contextShift(blocker.blockingContext))
  }

  def program(blocker: Blocker) = {
    implicit val t = IO.timer(exeContex)
    for {
      e1 <- infiniteIO(1)(blocker)
      e2 <- infiniteIO(2)(blocker)
      _ <- IO.sleep(1.second)
      _ <- e1.cancel
      _ <- e2.cancel
    } yield ()
  }
}
