package fp.in.scala

object LazyFree {
  sealed trait Free[F[_], A]

  final case class Return[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
  final case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B])
      extends Free[F, B]

  def ret[F[_], A](a: => A): Free[F, A] = Return(a)
  def suspend[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)
  def flatMap[F[_], A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    FlatMap(fa, f)

  object instances {
    implicit def freeMonad[F[_]: Monad]: Monad[Free[F, *]] =
      new Monad[Free[F, *]] {
        def ret[A](a: A): Free[F, A] = LazyFree.ret(a)
        def flatMap[A, B](m: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
          LazyFree.flatMap(m)(f)
        def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
          defaultTailRecM(a)(f)
      }
  }

}

object HaskellFree {
  def apply[F[_]](implicit instance: Monad[Free[F, *]]): Monad[Free[F, *]] = instance

  sealed trait Free[F[_], A]
  final case class Pure[F[_], A](a: A) extends Free[F, A]
  final case class Impure[F[_], A](run: F[Free[F, A]]) extends Free[F, A]

  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def eta[F[_]: Functor, A](f: F[A]): Free[F, A] =
    Impure(Functor[F].fmap(f)(pure _))

  object instances {
    implicit def freeMonad[F[_]: Functor]: Monad[Free[F, *]] =
      new Monad[Free[F, *]] {
        def ret[A](a: A) = HaskellFree.pure(a)
        def flatMap[A, B](m: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
          m match {
            case Pure(a)     => f(a)
            case Impure(run) => Impure(Functor[F].fmap(run)(flatMap(_)(f)))
          }
        @annotation.tailrec
        def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
          f(a) match {
            case Pure(either) =>
              either match {
                case Left(acc)    => tailRecM(acc)(f)
                case Right(value) => ???
              }
            case Impure(run) => ???
          }
      }

  }

}
