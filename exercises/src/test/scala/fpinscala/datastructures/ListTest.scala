package fpinscala.datastructures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("Test fold right length, 3") {
    assert(List.length(List(1, 2, 3)) == 3)
  }

  test("Test fold right length, 4") {
    assert(List.length(List(1, 2, 3, 4)) == 4)
  }

}
