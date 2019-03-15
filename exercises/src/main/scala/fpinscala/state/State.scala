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

    This is the interface for doing the work and contains the state. We call nextInt to advance the state
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

    This is essentially a type which represents going from one state to the next, including the result
   */
  type Rand[+A] = RNG => (A, RNG)

  /*
    This is shorthand for x => x.nextInt. As Rand: RNG => (A, RNG) and nextInt
    is (Int, RNG) and x is a RNG, this is RNG => (A, RNG)

    Note, this is a reference to a function itself
   */
  val int: Rand[Int] = _.nextInt

  /*
    This unit action passes the value along without using it
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /*
    This map action takes in a Rand, then from that rand gets the next value and
    RNG, then applies the function to the value.

    Remember

    type Rand[+A] = RNG => (A, RNG)

    So this represents something that takes in a state function and then applies a function to it to produce
    another state function. All of this is done on the functions themselves, no values are actually generated
   */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRng) = rng.nextInt
    if(int < 0) (int, nextRng) else nonNegativeInt(nextRng)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (int, nextRng) = nonNegativeInt(rng)
    (int % 2 == 0, nextRng)
  }

  /*
    Must be between zero and one
   */
  def double(rng: RNG): (Double, RNG) = {
    val (intOne, nextRngOne) = nonNegativeInt(rng)
    (intOne.toDouble / Int.MaxValue.toDouble, nextRngOne)
  }

  /*
    So
      Rand[+A] = RNG => (A, RNG)
    This returns a function that goes from an RNG to a (Double, RNG)
   */
  def doubleMap(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val(intVal, rngA) = rng.nextInt
    val(doubleVal, rngB) = double(rngA)
    ((intVal, doubleVal), rngB)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val((intVal, doubleVal), rngA) = intDouble(rng)
    ((doubleVal, intVal), rngA)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val(doubleValA, rngA) = double(rng)
    val(doubleValB, rngB) = double(rngA)
    val(doubleValC, rngC) = double(rngB)
    ((doubleValA, doubleValB, doubleValC), rngC)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(i: Int, l: List[Int])(rng: RNG): (List[Int], RNG) = {
      val (nextInt, nextRng) = rng.nextInt
      if(i <= 0) (l, rng) else go(i - 1, nextInt +: l)(nextRng)
    }
    go(count, List())(rng)
  }

  /*
    Combines two
      Rand[+A] = RNG => (A, RNG)
    According to a function f
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (raa, rngA) = ra(rng)
      val (rbb, rngB) = rb(rngA)
      (f(raa, rbb), rngB)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  /*

    So map looks like
      def map[A,B](s: Rand[A])(f: A => B): Rand[B]
    which essentially takes in a function that goes from RNG => (A, RNG) and
    a function which goes from A => B, then applies the function to the result
    of getting the next thing from the rng

    This does a similar thing, but rather than apply the function and return

    Rand[+A] = RNG => (A, RNG)

    We need to get the value from f, then apply g and return that

    g(nexInt)(nextRng):
      A => Rand[B]
      A => RNG => (B, RNG)
      A => (B, RNG)

   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (nextInt, nextRng) = f(rng)
      g(nextInt)(nextRng)
    }
  }

  /*
    So here we don't want to return a result, only a function to generate a result

   Remember

   def unit[A](a: A): Rand[A] =
    rng => (a, rng)
   */
  def nonNegativeLessThanFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(i) else nonNegativeLessThanFlatMap(i)
      }
    )
  }

  /*
    Difference between map and flatmap:

    Map:
      map[A,B](s: Functor[A])(f: A => B): Functor[B]
      Take a function from a to b, then a functor of a. Maps over Functor[A] and
      for each element applies f and rewraps it back in the Functor.

    FlatMap:
      flatMap[A,B](f: Monad[A])(g: A => Monad[B]): Monad[B]
      Take a function from a to a monad of B, then a monad of A. Extract from the
      monad of A, apply the function and return the monad of B.
   */

}

/*
  S is the state, A is the type we will return
 */
case class State[S,+A](run: S => (A, S)) {

  /*
    We have a function run  S => (A, S)
    We have a function f    A => B
    We want to return       State(S => (B, S))
                            S => (A, S)
   */
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  /*
    So we have
      run: S => (A, S)
      State(run) as a constructor

      f: A => State[S, B]

   */
  def flatMap[B](f: A => State[S, B]): State[S, B] = ???
}

object State {
  type Rand[A] = State[RNG, A]

  // Not needed in the class itself
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
