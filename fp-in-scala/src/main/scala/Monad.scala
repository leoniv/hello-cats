package fp.in.scala

trait Monad[F[_]] extends Functor[F] {
  def ret[A](a: => A): F[A]
  def flatMap[A, B](m: F[A])(f: A => F[B]): F[B]

  def unit[A](a: => A): F[A] = ret(a)
  def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))
  def fmap[A, B](f: F[A])(g: A => B): F[B] = map[A, B](f)(g)
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
  def doWhile[A](f: F[A])(p: A => F[Boolean]): F[Unit] =
    for {
      a <- f
      ok <- p(a)
      _ <- if (ok) unit(()) else doWhile(f)(p)
    } yield ()
  def as[A, B](fa: F[A])(b: B): F[B] = map(fa)(_ => b)
  def when[A](b: Boolean)(fa: F[A]): F[Boolean] =
    if (b) as(fa)(true) else ret(false)
  def foldM[A, B](l: Seq[A])(z: B)(f: (B, A) => F[B]): F[B] = l match {
    case h :: t => f(z, h) flatMap (z1 => foldM(t)(z1)(f))
  }

  def skip[A](fa: F[A]): F[Unit] = as(fa)(())
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
  def forever[A, B](fa: F[A]): F[B] = {
    lazy val loop: F[B] = for {
      _ <- fa
      b <- loop
    } yield b
    loop
  }

  protected def defaultTailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    flatMap(f(a)) {
      case Left(acc) => defaultTailRecM(acc)(f)
      case Right(r)  => ret(r)
    }

  implicit def toMonadic[A](a: F[A]): Monadic[F, A] =
    new Monadic[F, A] {
      val F = Monad.this
      def get = a
    }
}

trait Monadic[F[_], A] {
  val F: Monad[F]
  import F._
  def get: F[A]
  private val a = get
  def map[B](f: A => B): F[B] = F.map(a)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
//      def **[B](b: F[B]) = F.map2(a, b)((_, _))
//      def *>[B](b: F[B]) = F.map2(a, b)((_, b) => b)
  def map2[B, C](b: F[B])(f: (A, B) => C): F[C] = F.map2(a, b)(f)
  def >>[B](f: A => F[B]) = F.flatMap(a)(f)
  def as[B](b: B): F[B] = F.as(a)(b)
  def skip: F[Unit] = F.skip(a)
//      def replicateM(n: Int) = F.replicateM(n)(a)
//      def replicateM_(n: Int) = F.replicateM_(n)(a)
}

object Monad {
  def apply[F[_]](implicit instace: Monad[F]) = instace

  object instances {
    implicit def eitherMonad[L]: Monad[Either[L, *]] = new Monad[Either[L, *]] {
      def ret[A](a: => A): Either[L, A] = Right(a)
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

    implicit def function1Monad[X]: Monad[Function1[X, *]] =
      new Monad[Function1[X, *]] {
        def ret[A](a: => A): X => A = _ => a
        def flatMap[A, B](f: X => A)(g: A => (X => B)): X => B = x => g(f(x))(x)
        @annotation.tailrec
        def tailRecM[A, B](a: A)(f: A => (X => Either[A, B])): X => B =
          x => f(a)(x) match {
              case Left(a)  => tailRecM(a)(f)
              case Right(fb) => fb
            }
      }
  }

  object ops {
    implicit def toMonadOps[F[_], A](
        target: F[A]
    )(implicit tc: Monad[F]): Monadic[F, A] =
      new Monadic[F, A] {
        val F = tc
        def get = target
      }
  }
}
