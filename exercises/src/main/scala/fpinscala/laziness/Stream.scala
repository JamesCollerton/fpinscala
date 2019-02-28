package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  /*
    This is a lazy function
      z is a supplier function of type B, this is the default value at the start.
      f is a function which takes in a value of a and a supplier function of b and then gives a value of b
      The function returns a value of type b

    Because we have
      case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

    Then the head and tail are both lazy, as well as z, meaning that everything is lazily evaluated when the function
    is called
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List[A]())(_ :: _)

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) cons(h(), t().take(n - 1)) else Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n <= 0) cons(h(), t().drop(n)) else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b.takeWhile(p)) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = ???

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /*
    This is an alternative to Cons which takes in the supplier of head and tail and then lazily evaluates once
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs(s: Stream[Int]): Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] = {
      cons(i + j, go(j, j + 1))
    }
    go(0, 1)
  }

  def fibsUnfold(): Stream[Int] = unfold((0, 1))(t => Some((t._1 + t._2, (t._2, t._1 + t._2))))

  def fromUnfold(i: Int): Stream[Int] = unfold(i)(j => Some(j, j + 1))

  def constantUnfold(i: Int): Stream[Int] = unfold(i)(j => Some(j, j))

  /*
    Unfold is:
      Given a function from stream to an option
      If this function maps to a successful option then continue to unfold into the stream, otherwise return empty
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

}