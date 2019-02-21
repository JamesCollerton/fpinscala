package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case l: Leaf[A] => 1
    case b: Branch[A] => size(b.left) + size(b.right) + 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
      case l: Leaf[Int] => l.value
      case b: Branch[Int] => maximum(b.left) max maximum(b.right)
  }

  def maximumDepth[A](tree: Tree[A]): Int = tree match {
    case l: Leaf[A] => 1
    case b: Branch[A] => maximumDepth(b.left) max maximumDepth(b.right) + 1
  }

  def sizeFold(tree: Tree[Int]): Int = {
    fold(tree)(z => 1, (x, y) => x + y + 1)
  }

  def maximumFold(tree: Tree[Int]): Int = {
    fold(tree)(z => z.value, (x, y) => x max y)
  }

  def maximumDepthFold(tree: Tree[Int]): Int = {
    fold(tree)(z => 1, (x, y) => x max y + 1)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case l: Leaf[A] => Leaf(f(l.value))
    case b: Branch[A] => Branch(map(b.left)(f), map(b.right)(f))
  }

  def fold(tree: Tree[Int])(f: Leaf[Int] => Int, g: (Int, Int) => Int): Int = tree match {
    case l: Leaf[Int] => f(l)
    case b: Branch[Int] => g(fold(b.left)(f, g), fold(b.right)(f, g))
  }

}