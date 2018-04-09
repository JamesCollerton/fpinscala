//package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 1
		case Branch(l, r) => size(l) + size(r) + 1
	}

	def maximum(tree: Tree[Int]): Int = tree match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	def depth(tree: Tree[Int]): Int = tree match {
		case Leaf(v) => 1
		case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
	}

	def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
		case Leaf(v) => f(v)
		case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}

	def foldSize[A](tree: Tree[A]): Int = {
		fold(tree)(v => 1)((l, r) => l + r + 1)
	}

	def foldMaximum(tree: Tree[Int]): Int = {
		fold(tree)(v => v)((l, r) => l max r)
	}

	def foldDepth(tree: Tree[Int]): Int = {
		fold(tree)(v => 1)((l, r) => (l max r) + 1)
	}

	def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
		fold[A, Tree[B]](tree)(v => Leaf(f(v)))((l, r) => Branch(l, r))
	}
	
	def printTree[A](tree: Tree[A]): Unit = tree match {
		case Leaf(v) => println("/" + v)
		case Branch(l, r) => {
			printTree(l)
			printTree(r)
		}
	}

}

object TestTree {
	
	def main(args: Array[String]): Unit = {
		val tree = setUpTree()

		println("Size " + Tree.size(tree))
		println("Maximum " + Tree.maximum(tree))
		println("Depth " + Tree.depth(tree))
		Tree.printTree(Tree.map(tree)(a => a * a))
	
		println()
	
		println("Fold Size " + Tree.foldSize(tree))
		println("Fold Maximum " + Tree.foldMaximum(tree))
		println("Fold Depth " + Tree.foldDepth(tree))
		Tree.printTree(Tree.foldMap(tree)(a => a * a))
	}

	def setUpTree(): Tree[Int] = {
		val l1 = Leaf(1)
		val l2 = Leaf(2)
		val l3 = Leaf(3)
		val l4 = Leaf(4)

		val b1 = Branch(l1, l2)
		val b2 = Branch(l3, l4)

		Branch(b1, b2)
	}

}
