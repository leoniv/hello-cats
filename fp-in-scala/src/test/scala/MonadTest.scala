package fp.in.scala
import org.specs2.mutable.Specification

class MonadTest extends Specification {
  import Monad.instances._
  import Monad.ops._
  import syntax._
  "Monad exmples".p
  eg { Monad[EitherN].ret(1) must beRight(1) }
  eg { Monad[EitherN].flatMap(1.right)((i:Int) => Monad[EitherN].ret(i + 1)) must beRight(2) }
  eg { Monad[EitherI].flatMap(1.left)((i:Int) => Monad[EitherI].ret(i + 1)) must beLeft(1) }
  eg { Monad[EitherN].ret(1) *> 2.right must beRight(2) }
  eg { Monad[EitherN].ret(1) <* 2.right must beRight(1) }
  eg { (1.left) flatMap ((i:Int) => Monad[EitherI].ret(i + 1)) must beLeft(1) }
}

