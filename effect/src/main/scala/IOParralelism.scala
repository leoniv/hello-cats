package hello_cats.effect

import cats.effect.{ExitCase, ContextShift, IO}
import cats.implicits._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object IOParralelism {
  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)
  implicit val timer = IO.timer(ExecutionContext.global)

  def parralelExec = {
    val ioA = IO(println("Running ioA"))
    val ioB = IO(println("Running ioB"))
    val ioC = IO(println("Running ioC"))

    val program = (ioA, ioB, ioC).parMapN { (_, _, _) => () }

    program
  }

  def canseled = {
    val a =
      IO.sleep(10.second) *> IO.raiseError[Unit](new Exception("boom")) <* IO(println("Running ioA"))
    val b = (IO(println("Running ioB")))
      .guaranteeCase {
        case ExitCase.Canceled => IO(println("ioB was canceled!"))
        case _                 => IO.unit
      }

      val program = (b, a).parMapN { (_, _) => () }
      program
  }
}
