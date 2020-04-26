package fp.in.scala

trait Monad[F[_]] {
  def ret[A](a: A): F[A]
  def flatMap[A, B](m: F[A])(f: A => F[B]): F[B]
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
  protected def defaultTailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    flatMap(f(a)) {
      case Left(acc) => defaultTailRecM(acc)(f)
      case Right(r)  => ret(r)
    }
}

object Monad {
  def apply[F[_]](implicit instace: Monad[F]) = instace

  object instances {
    implicit def eitherMonad[L]: Monad[Either[L, *]] = new Monad[Either[L, *]] {
      def ret[A](a: A): Either[L, A] = Right(a)
      def flatMap[A, B](m: Either[L, A])(f: A => Either[L, B]): Either[L, B] =
        m match {
          case Left(value)  => Left(value)
          case Right(value) => f(value)
        }
      @annotation.tailrec
      def tailRecM[A, B](a: A)(f: A => Either[L, Either[A, B]]): Either[L, B] =
        f(a) match {
          case Left(e) => Left(e)
          case Right(value) =>
            value match {
              case Left(acc) => tailRecM(acc)(f)
              case Right(r)  => ret(r)
            }
        }
    }
  }

  object ops {
    implicit def toMonadOps[F[_], A](target: F[A])(implicit tc: Monad[F]): Ops[F, A] =
      new Ops[F, A] {
        def typeClassInstance = tc
        def self = target
      }
  }

  trait Ops[F[_], A] {
    def typeClassInstance: Monad[F]
    def self: F[A]
    def flatMap[B](f: A => F[B]) = typeClassInstance.flatMap[A, B](self)(f)
    def >>[B](f: A => F[B]) = flatMap(f)
  }
}
