package fpinscala.datastructures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("Test fold right length, 3") {
    assert(List.length(List(1, 2, 3)) == 3)
  }

  test("Test fold right length, 4") {
    assert(List.length(List(1, 2, 3, 4)) == 4)
  }

  test("Test fold left sum 6") {
    assert(List.sumFoldLeft(List(1, 2, 3)) == 6)
  }

  test("Test fold left product 6") {
    assert(List.productFoldLeft(List(1, 2, 3)) == 6)
  }

  test("Test fold left length 3") {
    assert(List.lengthFoldLeft(List(1, 2, 3)) == 3)
  }

  test("Reverse list 1, 2, 3 gives 3, 2, 1") {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("Append list 1, 2, 3 and 4 gives 1, 2, 3, 4") {
    assert(List.append(List(1, 2, 3), 4) == List(1, 2, 3, 4))
  }

}
