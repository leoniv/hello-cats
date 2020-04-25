package fp.in.scala

import org.specs2.mutable.Specification
import Functor.instances._
import Functor.ops._

class FunctorSpec extends Specification {
  import syntax._

  "With object Functor examples".p
  eg { Functor[EitherN].fmap(1.right) { _ + 1 } must beRight(2) }
  eg { Functor[EitherI].fmap(1.left[Int]) { _ + 1 } must beLeft(1) }
  eg { Functor[EitherN].liftF((_: Int) + 1)(1.right) must beRight(2) }

  "With syntax Functor exmples".p
  eg { 1.right[Nothing] fmap { _ + 1 } must beRight(2) }
  eg { 1.left[Int] fmap { _ + 1 } must beLeft(1) }
  eg { 1.some fmap ((_: Int) + 1) must beSome(2) }
  eg { 1.none fmap ((_: Int) + 1) must beNone }

}
