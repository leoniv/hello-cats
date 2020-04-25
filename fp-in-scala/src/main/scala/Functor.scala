package fp.in.scala

trait Functor[F[_]] {
  def fmap[A, B](f: F[A])(g: A => B): F[B]
  def liftF[A, B](g: A => B): F[A] => F[B] = fmap(_)(g)
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]) = F

  object instances {
    implicit def eitherFunctor[A]: Functor[Either[A, *]] =
      new Functor[Either[A, *]] {
        def fmap[B, C](f: Either[A, B])(g: B => C): Either[A, C] = f.map(g)
      }

    implicit def optionFunctor: Functor[Option[*]] =
      new Functor[Option[*]] {
        def fmap[A, B](f: Option[A])(g: A => B): Option[B] = f.map(g)
      }
  }

  object ops {
    implicit def functorOps[F[_], A](
        target: F[A]
    )(implicit tc: Functor[F]): Ops[F,A] = new Ops[F,A] {

      def typeClassInstance = tc
      def self = target
    }
  }

  trait Ops[F[_], A] {
    def typeClassInstance: Functor[F]
    def self: F[A]
    def fmap[B](f: A => B): F[B] = typeClassInstance.fmap(self)(f)
    def liftF[B](f: A => B): F[A] => F[B] = typeClassInstance.liftF(f)
    def <%> = liftF _
  }
}
