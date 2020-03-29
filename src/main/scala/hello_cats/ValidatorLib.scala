package hello_cats

import cats.data.ReaderT
import cats.data.Validated
import cats.data.NonEmptyList
import cats.syntax.validated._
import cats.syntax.apply._
import cats.kernel.Semigroup

object ValidatorLib {
  type Error = NonEmptyList[String]
  type ValidatedL[A] = Validated[Error, A]
  type Input[A, B] = ReaderT[ValidatedL, A, B]

  sealed trait Predicate[E, A] {
    import Predicate._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A]
    def run(a: A)(implicit s: Semigroup[E]): Either[E, A] = this(a).toEither
    def or(other: Predicate[E,A]) = Or(this, other)
    def and(other: Predicate[E,A]) = And(this, other)
  }

  object Predicate {
    case class Pure[E, A](fn: A => Validated[E,A]) extends Predicate[E, A] {
      def apply(a: A)(implicit s: Semigroup[E]) = fn(a)
    }

    case class Or[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]) extends Predicate[E, A] {
      def apply(a: A)(implicit s: Semigroup[E]) = ???
    }

    case class And[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]) extends Predicate[E, A] {
        def apply(a: A)(implicit s: Semigroup[E]) =
          (left(a), right(a)).mapN{ (_, _) => a }
    }

    def apply[E, A](fn: A => Validated[E, A]) = Pure(fn)
    def lift[E, A](error: E, p: A => Boolean) = Pure( (a: A) =>
          if (p(a)) a.valid else error.invalid
        )
  }

  def error(m: String) = NonEmptyList(m, Nil)

  def longerThan(n: Integer) = ???
}
