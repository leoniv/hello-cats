package fp.in.scala

import org.specs2.mutable.Specification
import fp.in.scala.Applicative

class ApplicativeSpec extends Specification {
  import syntax._
  import Applicative.instances._
  import Applicative.ops._
  import Functor.instances._

  "Applicative example".p
  eg { Applicative[EitherN].pure[Int](1) must beRight(1) }
  eg { Applicative[EitherN].pure((x: Int) => (y: Int) => x + y) <*> (1.right) <*> (2.right) must beRight(3) }
  eg { Applicative[EitherN].liftA2((x: Int, y: Int) => x + y)(1.right)(2.right) must beRight(3) }
  eg { Applicative[EitherN].liftA2((x: Int) => (y: Int) => x + y)(1.right)(2.right) must beRight(3) }
  eg { Applicative[EitherN].ap(Applicative[EitherN].pure((_: Int) + 1))(1.right) must beRight(2) }
}
