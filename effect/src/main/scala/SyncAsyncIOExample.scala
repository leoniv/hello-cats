package hello_cats.effect

import cats.effect._
import cats.implicits._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object SyncAsyncIOExample {
  implicit val ecGlob: ExecutionContext = ExecutionContext.global
  implicit val timer: Timer[IO] = IO.timer(ecGlob)
  implicit val cs: ContextShift[IO] = IO.contextShift(ec = ecGlob)
  def syncIo(mess: String): IO[String] = IO { println(mess) } *> IO { mess }
  def asyncIo(mess: String): IO[String] = IO.async { cb => cb(mess.asRight) }

  def sendMessage(message: String) =
    for {
      mess <- asyncIo(message).start
      _ <- (IO.sleep(3.second) *> IO(println("sleeping End")))
        .runAsync(_ => IO.unit)
        .toIO
      mess <- mess.join
      ret <- syncIo(mess)
    } yield ret
}
