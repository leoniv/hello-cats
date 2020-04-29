package fp.in.scala

import org.specs2.mutable.Specification

class FreeSpec extends Specification {
  eg { FreeMonad.TailRecExample.runTrampolineWith("Hello") must beEqualTo("Mapped Hello")}
  eg { FreeMonad.TailRecExample.runWith("Hello") must beEqualTo("Mapped Hello")}
}
