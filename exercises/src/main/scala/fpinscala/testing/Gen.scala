package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
  The library developed in this chapter goes through several iterations. This file is just the
  shell, which you can fill in and modify while working through the chapter.

  This trait represents a property we would like to check. We can check the property by running the
  check method.
*/

trait Prop {
  // Running a property
  def check: Boolean = ???

  // Composing a property
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = this.check && p.check
  }

  /*
    Creating a property.

    This takes a generator and a function, then using the generator returns a property
    matching the function.

    For example, generator generates a random number, function is is positive, the property
    will be check the generator for the number being positive
  */
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Prop {

}

/*
  This class is used to generate random numbers

  case class State[S,+A](run: S => (A, S))
 */
case class RNGen[A](sample: State[RNG,A]) {

}

object Gen {

  def choose(start: Int, stopExclusive: Int): RNGen[Int] = {
    /*
      Where A is RNG:

      Map has signature:
        map[B](f: A => B): State[S, B]

      Flatmap has signature:
        def flatMap[B](f: A => State[S, B]): State[S, B]

      The function needs to be of type
        RNG => (Int, RNG)

      Map we can essentially use to extract the value from the state, apply something to it and then put it back
     */
    RNGen[Int](State(s => RNG.nonNegativeInt(s)).map(i => i + start % (stopExclusive - start)))
  }

  // This is a lazily evaluated function which takes in a supplier of A
  // and then returns a Gen
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  // This is used to generate a boolean
  def boolean: Gen[Boolean] = Gen(State(rng => RNG.boolean(rng)))

  /*
    This is used to generate a list of length n of type A using a generator

    So
      Gen[A](sample: State[RNG,A])
      State[S,+A](run: S => (A, S))

    Then
      Gen[List[A]](sample: State[RNG, List[A]])
      State[S, List[A]](run: S => (List[A], S))
    */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???

//  {
//    def go(n: Int, gl: Gen[List[A]]): Gen[List[A]] = {
//      val g.
//      if(n == 0) gl else g.flatMap(a => )
//    }
//
//    val origList = g.map(a => List(a))
//  }

}

/*
  Remember that the constructor for state is

    State[S,+A](run: S => (A, S))

  So this is RNG => (RNG, A)
 */
case class Gen[A](sample: State[RNG,A]) {

  /*
    So remember that map takes out the value, applies the function then puts it back into the class
   */
  def map[A,B](f: A => B): Gen[B] = ???

  /*
    This
   */
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???

}

trait SGen[+A] {

}

