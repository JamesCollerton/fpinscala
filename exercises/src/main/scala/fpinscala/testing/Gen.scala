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
*/

trait Prop {
  // Running a property
  def check: Boolean = ???

  // Composing a property
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = this.check && p.check
  }

  // Creating a property. The property is essentially
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Prop {

}

object Gen {
  // This is a lazily evaluated function which takes in a supplier of A
  // and then returns a Gen
  def unit[A](a: => A): Gen[A] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

