package fp.in.scala

object FreeMonad {
  sealed trait Free[F[_], A] { self =>
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(self, f)
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen { Pure(_) })
  }
  final case class Pure[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
  final case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B])
      extends Free[F, B]

  object Free {
    def apply[F[_], A](a: A): Free[F, A] = Pure(a)

    implicit def freeMonadInstance[F[_]: Monad]: Monad[Free[F, *]] = ???
  }

  object TailRecExample {
    type TailRec[A] = Free[Function0, A]

    @annotation.tailrec
    def runTrampoline[A](tr: TailRec[A]): A = tr match {
      case Pure(a)         => a
      case Suspend(resume) => resume()
      case FlatMap(fa, f) =>
        fa match {
          case Pure(a)         => runTrampoline(f(a))
          case Suspend(resume) => runTrampoline(f(resume()))
          case FlatMap(faa, g) =>
            runTrampoline(faa flatMap (x => g(x) flatMap f))
        }
    }

    def program(s: String): TailRec[String] =
      for {
        s <- Pure(s)
        _ <- Suspend(() => println(s"Echo $s"))
      } yield s
  }
}
