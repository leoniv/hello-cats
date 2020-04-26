package fp.in.scala
import org.specs2.mutable.Specification

class MonadTest extends Specification {
  import Monad.instances._
  import Monad.ops._
  import syntax._
  "Monad exmples".p
  eg { Monad[EitherN].ret(1) must beRight(1) }
  eg { (1.left) flatMap ((i:Int) => Monad[EitherI].ret(i + 1)) must beLeft(1) }
  eg { (1.right[Nothing]) >> ((i:Int) => Monad[EitherN].ret(i + 1)) must beRight(2) }
}

