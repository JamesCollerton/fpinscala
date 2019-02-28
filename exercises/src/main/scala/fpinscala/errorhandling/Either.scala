package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

/*
  Either is used when we want to signal an error. There is a left value (the error value) and a right value (the successful
  value). We then have a number of functions:

  - map: If we are currently successful (i.e. left is not populated) then we apply the mapped function and return a right
  of the successful value. Otherwise just return the left value.

  - flatMap: This is more powerful than map. In map we allow the map function to put the value back into an optional. In this
  case we take responsibility for putting it back into the optional
 */
sealed trait Either[+E,+A] {

  /*
    So here the function takes responsibility for putting the result of the function back into the type.
   */
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(e) => Left(e)
 }

  /*
    Here the function we pass to flatmap takes the responsibility for putting the result back into the type
   */
 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => f(a)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => Right(a)
   case Left(a) => b
 }

  /*
  So map2 is used for combining this and another instance of this using a function
   */
 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
   case (Right(aa), Right(ab)) => Right(f(aa, ab))
   case (_, Left(eb)) => Left(eb)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  /*
    Traverse is getting a list and a function and then using that function to map from the current list
    to another list which contains a Right of B.
   */
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h).flatMap(newH => traverse(t)(f).map(newT => newH :: newT))
  }

  /*
    This is used to convert a list of eithers into an either of lists
   */
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h.map2(sequence(t))(_ :: _)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  /*
    Wrapping exceptions in an either
   */
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}