package fp.in.scala

import org.specs2.mutable.Specification

class IOSimpleSpec extends Specification {
  eg { IOSimple.program.run must beEqualTo(()) }
}
