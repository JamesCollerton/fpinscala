package fpinscala.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("To list 1, 2, 3") {
    assert(Stream("1", "2", "3").toList == List("1", "2", "3"))
  }

  test("Take 1, 2, 3 first 2") {
    assert(Stream("1", "2", "3").take(2).toList == List("1", "2"))
  }

  test("Drop 1, 2, 3 first 2") {
    assert(Stream("1", "2", "3").drop(2).toList == List("3"))
  }

  test("Take while 1, 2, 3 not 3") {
    assert(Stream("1", "2", "3").takeWhile(_ == "3").toList == List("1", "2"))
  }
}
