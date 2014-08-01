package fpinscala.gettingstarted

import org.specs2.mutable._

class GettingStartedSpec extends Specification {

  "The 'Hello world' string" should {
    "contain 11 characters" in {
      "Hello world" must have size(11)
    }
    "start with 'Hello'" in {
      "Hello world" must startWith("Hello")
    }
    "end with 'world'" in {
      "Hello world" must endWith("world")
    }
  }
  "fib" should {
    "return 3 when 5 given" in {
      MyModule.fib(5) must_== 3
    }
    "return 0 when 1 given" in {
      MyModule.fib(1) must_== 0
    }
  }
  "isSorted" should {
    "return true" in {
      PolymorphicFunctions.isSorted(Array(1, 2), ((left: Int, right: Int) => left < right)) must_== true
    }
    "return false" in {
      PolymorphicFunctions.isSorted(Array(1, 2), ((left: Int, right: Int) => left > right)) must_== false
    }
  }
}
