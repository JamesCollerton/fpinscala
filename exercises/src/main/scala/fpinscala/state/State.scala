package fpinscala.state

/*
  Trait of random number generator:

    Objects: A singleton accessed directly, instantiated when accessed for the first time.

    Trait: A class that allows multiple inheritance, essentially an interface but can have
    method bodies, variables etc. You can't have arguments in the constructor.

    Class: An actual class

    Case Class: A class that allows for pattern matching

    Note on the apply method: This is simply what is called when you invoke an object by
    name. WE DON'T USE new AS THIS CREATES AN INSTANCE OF THE CLASS.
 */
trait RNG {

  /*
    Function to get the next integer. As this is a trait this is the same as having a
    method declaration with no body
   */
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

/*
  Companion object
 */
object RNG {
  // NB - this was called SimpleRNG in the book text

  /*
    This function is used to generate a random integer and return the next
    random number generator with a new state.
   */
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  /*
    Defines a type, Rand is just a function that goes from an RNG
    to a pair of type (A, RNG), giving the next value and the next RNG
   */
  type Rand[+A] = RNG => (A, RNG)

  /*
    This is shorthand for x => x.nextInt. As Rand: RNG => (A, RNG) and nextInt
    is (Int, RNG) and x is a RNG, this is RNG => (A, RNG)
   */
  val int: Rand[Int] = _.nextInt

  /*
    This unit action passes the RNG along without using it
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /*
    This map action takes in a Rand, then from that rand gets the next value and
    RNG, then applies the function to the value.
   */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  def double(rng: RNG): (Double, RNG) = ???

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
