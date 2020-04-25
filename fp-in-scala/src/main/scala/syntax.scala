package fp.in.scala

package object syntax extends {
  type EitherN[A] = Either[Nothing, A]
  type EitherI[A] = Either[Int, A]

  implicit class IntToEither(i: Int) {
    def left[A]: Either[Int, A] = Left(i)
    def right[A]: Either[A, Int] = Right(i)
  }

  implicit class IntToOption(i: Int) {
    def some: Option[Int] = Some(i)
    def none: Option[Int] = None
  }
}
