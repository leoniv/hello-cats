package hello_cats

import cats.data.ReaderT
import cats.data.Validated
import cats.data.NonEmptyList
import cats.syntax.validated._
import cats.syntax.apply._
import cats.kernel.Semigroup
import cats.data.Validated._
import Function.const

object ValidatorLib {
  type Errors = NonEmptyList[String]
  type Result[A] = Either[Errors, A]
  type Check[A, B] = ReaderT[Result, A, B]
  type Input[A] = Map[String, A]

  sealed trait Predicate[E, A] {
    import Predicate._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A]
    def run(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this(a)
    def or(other: Predicate[E,A]) = Or(this, other)
    def and(other: Predicate[E,A]) = And(this, other)
    def contramap[B](f: B => A)(implicit s: Semigroup[E]): Predicate[E, B] =
      Pure((b: B) => apply(f(b)) map (const(b)))
  }

  object Predicate {
    case class Pure[E, A](fn: A => Validated[E,A]) extends Predicate[E, A] {
      def apply(a: A)(implicit s: Semigroup[E]) = fn(a)
    }

    case class Or[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]) extends Predicate[E, A] {
      def apply(a: A)(implicit s: Semigroup[E]) = left(a) match {
        case Valid(_) => Valid(a)
        case Invalid(e1) => right(a) match {
          case Valid(_) => Valid(a)
          case Invalid(e2) => Invalid(s.combine(e1, e2))
        }
      }
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

  def check[A, B](fn: A => Result[B]): Check[A, B] = ReaderT(fn)

  def pred2check[A](p: Predicate[Errors, A]): Check[A, A] = check((a: A) =>
      p(a).toEither
    )

//FIXME  def checkCombine[A, B](p: Predicate[Errors, A]): Check[A, B]

  def check2pred[A](ch: Check[A, _]): Predicate[Errors, A] =
    Predicate((a: A) => Validated.fromEither(ch.run(a)) map const(a))

  def error(m: String) = NonEmptyList(m, Nil)

  private def keyNotFoundError(k: String) = error(s"Key `$k' not found")

  def readKey[A](k: String): Check[Input[A], A] = check( (m: Input[A]) =>
    m.get(k).toRight(keyNotFoundError(k))
  )

  def mustBeKey(k: String): Predicate[Errors, Input[_]] =
    check2pred(readKey(k))

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def isInt: Predicate[Errors, String] =
    Predicate.lift(
      error("Must be a number"),
      str => """^-?\d+""".r.matches(str)
    )


  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)
}
