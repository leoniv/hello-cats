package fp.in.scala

import org.specs2.mutable.Specification
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

class ParSpec extends Specification {
  val es: ExecutorService = Executors.newFixedThreadPool(2)

  eg { Par.run(es)(Par.map2(Par.lazyUnit(1), Par.unit(2))(_ + _)).get must beEqualTo(3) }
}
