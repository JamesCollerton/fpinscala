package fpinscala.datastructures

import org.scalatest.{BeforeAndAfter, FunSuite}

class TreeTest extends FunSuite with BeforeAndAfter {

  val leaf1 = Leaf(1)
  val leaf2 = Leaf(2)
  val leaf3 = Leaf(3)
  val leaf4 = Leaf(4)

  val branchLeft = Branch(leaf1, leaf2)
  val branchRight = Branch(leaf3, leaf4)

  val tree = Branch(branchLeft, branchRight)

  val leaf6 = Leaf(6)
  val leaf8 = Leaf(8)

  val branchLeftDouble = Branch(leaf2, leaf4)
  val branchRightDouble = Branch(leaf6, leaf8)

  val treeDouble = Branch(branchLeftDouble, branchRightDouble)

  test("Size tree is 7") {
    assert(Tree.size(tree) == 7)
  }

  test("Maximum is 4") {
    assert(Tree.maximum(tree) == 4)
  }

  test("Maximum depth is 3") {
    assert(Tree.maximumDepth(tree) == 3)
  }

  test("Size fold tree is 7") {
    assert(Tree.sizeFold(tree) == 7)
  }

  test("Maximum fold is 4") {
    assert(Tree.maximumFold(tree) == 4)
  }

  test("Maximum depth fold is 3") {
    assert(Tree.maximumDepthFold(tree) == 3)
  }

  test("Map double doubles tree") {
    assert(Tree.map(tree)(_ * 2) == treeDouble)
  }

}
