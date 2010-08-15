package org.no.ip.bca.superlearn

import org.specs._

object FastRandomSpec extends FastRandomTest
class FastRandomTest extends SpecificationWithJUnit {
  "nextDouble" should {
    def run(f: (Double) => Unit) = {
      for(i <- 0 until 10) {
        val fr = new FastRandom
        for(i <- 0 until 10) {
          f(fr.nextDouble)
        }
      }
    }
    "must be >= 0.0" >> run { d =>
      d must beGreaterThanOrEqualTo(0.0)
    }
    "must be < 1.0" >> run { d =>
      d must beLessThan(1.0)
    }
  }
}