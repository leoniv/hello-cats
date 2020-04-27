package fp.in.scala

import org.specs2.mutable.Specification

class TailRecSpec extends Specification {
  eg { TailRecExample.processLongLoop should_== (42) }
}
