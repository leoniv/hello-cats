package fp.in.scala

import org.specs2.mutable.Specification

class HaskellFreeSpec extends Specification {
  import syntax._
  import Functor.instances._
  import HaskellFree._
  import HaskellFree.instances._

  "Smart constructors".p
  eg { HaskellFree[EitherN].ret(1) must equalTo(Pure(1)) }
  //eg { eta(1.some) flatMap ((i: Int) => Pure(i + 1)) must beEqualTo("OK") }
//  eg {
//    HaskellFree.flatMap(HaskellFree.suspend(1.left))((i: Int) => HaskellFree.ret(i + 1)) must beLike {
//      case FlatMap(m, f) => ok
//    }
//  }
}
