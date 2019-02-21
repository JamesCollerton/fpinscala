package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case l: Leaf[A] => 1
    case b: Branch[A] => size(b.left) + size(b.right) + 1
  }

  def maximum(tree: Tree[Int]): Int = {
    def maximum(tree: Tree[Int], maxNumber: Option[Int]): Int = tree match {
      case l: Leaf[Int] => maxNumber.map(x => x max l.value).getOrElse(l.value)
      case b: Branch[Int] => maximum(b.left, maxNumber) max maximum(b.right, maxNumber)
    }
    maximum(tree, Option.empty)
  }

  def maximumDepth[A](tree: Tree[A]): Int = tree match {
    case l: Leaf[A] => 1
    case b: Branch[A] => maximumDepth(b.left) max maximumDepth(b.right) + 1
  }

}