package fpinscala.gettingstarted

import org.specs2.mutable._

class GettingStartedSpec extends Specification {
  "abs" should {
    "return absolute value" in {
      MyModule.abs(-1) mustEqual 1
    }
  }
}

