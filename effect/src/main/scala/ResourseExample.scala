package hello_cats.effect

import cats.effect._
import cats.implicits._
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.duration._

object ResourseExample {

  implicit val blocker =
    Blocker.liftExecutorService(Executors.newSingleThreadExecutor())
  implicit val csGlobal = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  def sleepinIo(duration: Int): IO[Int] =
    (IO.sleep(duration.second) *> IO { println(show"Sleeping end") } *> IO(
      duration
    )).runAsync(_ => IO.unit).toIO.as(duration)
  final def withResource[A](r: Resource[IO, A])(fs: A => List[Any]): List[Any] =
    r match {
      case Resource.Allocate(resource) =>
        resource.map {
          case (a, release) =>
            release(ExitCase.Completed).unsafeRunSync :: fs(a)
        }.unsafeRunSync
      case Resource.Suspend(fa) => withResource(fa.unsafeRunSync)(fs)
      case Resource.Bind(source, f) =>
        withResource(source)(s => withResource(f(s))(fs))
    }
  def resourse(duration: Int): Resource[IO, Int] =
    for {
      i <- Resource.make(sleepinIo(duration))(i =>
        IO { println(show"Release $i") }
      )
    } yield i

  def forceEval = withResource(resourse(3).flatMap(_ => resourse(4)))(i => List(i))

  def program = for {
    i <- resourse(3).use(i => IO (println(s"Use $i")) *> IO(i) )
  } yield i
}
