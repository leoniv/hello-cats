package fp.in.scala

import org.specs2.mutable.Specification
import Functor.instances._
import Functor.ops._

class FunctorSpec extends Specification {
  type EitherN[A] = Either[Nothing, A]

  implicit class IntToEither(i: Int) {
    def left[A]: Either[Int, A] = Left(i)
    def right[A]: Either[A, Int] = Right(i)
  }

  "With object Functor examples".p
  eg { Functor[EitherN].fmap(1.right) { _ + 1 } must beRight(2) }
  eg { Functor[EitherN].liftF((_: Int) + 1)(1.right) must beRight(2) }

  "With syntax Functor exmples".p
  eg { 1.right fmap { _ + 1 } must beRight(2) }
  //eg { Some(1) <%> ((_: Int) + 1) must beSome(2) }

}
