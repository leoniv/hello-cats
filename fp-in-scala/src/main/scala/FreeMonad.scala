package fp.in.scala
import fp.in.scala.Monad.ops._

object FreeMonad {
  sealed trait Free[F[_], A] { self =>
    def run(implicit F: Monad[F]) = Free.run(self)
  }
  final case class Pure[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
  final case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B])
      extends Free[F, B]

  object Free {
    def apply[F[_], A](a: A): Free[F, A] = pure(a)

    def pure[F[_], A](a: A): Free[F, A] = Pure(a)
    def suspend[F[_], A](fa: F[A]) = Suspend(fa)
    def flatMap[F[_], A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      FlatMap(fa, f)

    def run[F[_], A](fr: Free[F, A])(implicit F: Monad[F]): F[A] =
      step(fr) match {
        case Pure(a)                     => F.ret(a)
        case Suspend(resume)             => F.map(resume)(a => a)
        case FlatMap(Suspend(resume), f) => F.flatMap(resume)(a => run(f(a)))
        case _                           => sys.error("FTF? Int's impossible!")
      }

    @annotation.tailrec
    def step[F[_], A](fr: Free[F, A]): Free[F, A] = fr match {
      case FlatMap(Pure(a), f)        => step(f(a))
      case FlatMap(FlatMap(fa, f), g) => step(fa flatMap (a => f(a) flatMap g))
      case _                          => fr
    }

    implicit def toMonad[F[_]]: Monad[Free[F, *]] =
      new Monad[Free[F, *]] {
        def ret[A](a: => A): Free[F, A] = Free.pure(a)
        def flatMap[A, B](fr: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
          Free.flatMap(fr)(f)
        def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
          ???
      }
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

    implicit def function0Monad: Monad[Function0] = new Monad[Function0] {
      def ret[A](a: => A): () => A = () => a
      def flatMap[A, B](m: () => A)(f: A => (() => B)): () => B = f(m())
      def tailRecM[A, B](a: A)(f: A => (() => Either[A, B])): () => B = ???
    }

    def pure[A](a: A): TailRec[A] = Free.pure(a)
    def suspend[A](f: () => A): TailRec[A] = Free.suspend(f)

    def program(s: String): TailRec[String] =
      for {
        s <- pure(s)
        _ <- suspend(() => println(s"Echo $s"))
      } yield s

    def runTrampolineWith(s: String) = runTrampoline(program(s).map(s => s"Mapped $s"))
    def runWith(s: String) = program(s).map(s => s"Mapped $s").run.apply()
  }
}
