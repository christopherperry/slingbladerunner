import org.scalatest.FunSuite

class ChainHelperTest extends FunSuite {
  implicit def stringWrapper(movieTitle: String) = new ChainHelper(movieTitle)

  test("canChain: should check for chaining to the right") {
    assert("sling blade" canChain "blade runner")
    assert(!("blade runner" canChain "sling blade"))
  }

  test("canChain: should check for multiple word overlap") {
    assert("one two three" canChain "two three four five")
    assert("one two three four" canChain "two three four five")
    assert(!("one two three four six" canChain "two three four five"))
  }

  test("canChain: should not chain these") {
    assert(!("AN INCONVENIENT TRUTH" canChain "WHERE THE TRUTH LIES"))
  }

  test("canChain: should chain these") {
    assert("EARTH GIRLS ARE EASY" canChain "EASY RIDER")
  }
}
