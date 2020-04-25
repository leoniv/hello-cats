package fp.in.scala

import fp.in.scala.Functor

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](f: F[A => B])(a: F[A]): F[B]
  def liftA[A, B](f: A => B)(a: F[A]): F[B] = ap(pure(f))(a)
  def liftA2[A, B, C](f: A => B => C)(a: F[A])(b: F[B]): F[C] = ap(liftA(f)(a))(b)
  def liftA2[A, B, C](f: (A, B) => C)(a: F[A])(b: F[B]): F[C] = liftA2(f.curried)(a)(b)
}

object Applicative {
  import Functor.instances._

  def apply[F[_]](implicit instance: Applicative[F]) = instance

  object instances {
    implicit def eitherApplicative[L]: Applicative[Either[L, *]] =
      new Applicative[Either[L, *]] with Functor[Either[L, *]] {
        def pure[A](a: A): Either[L, A] = Right(a)
        def ap[A, B](f: Either[L, A => B])(a: Either[L, A]): Either[L, B] =
          f match {
            case Left(e)  => Left(e)
            case Right(f) => fmap(a)(f)
          }
        def fmap[A, B](f: Either[L, A])(g: A => B): Either[L, B] =
          eitherFunctor[L].fmap(f)(g)
      }

  }

  object ops {
    implicit def applicativeOps[F[_], A, B](
        target: F[A => B]
    )(implicit tc: Applicative[F]): Ops[F, A, B] = new Ops[F, A, B] {
      def typeClassInstance = tc
      def self = target
    }
  }

  trait Ops[F[_], A, B] {
    def typeClassInstance: Applicative[F]
    def self: F[A => B]
    def ap(a: F[A]): F[B] = typeClassInstance.ap(self)(a)
    def <*> = ap _
  }
}
