import cats.effect._
import cats.implicits._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO(println("Boom!"))
  } yield ExitCode.Success
}
