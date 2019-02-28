package fpinscala.laziness

object LazyExample {

  def main(args: Array[String]): Unit = {
    println(lazyFunction(1))
  }

  def lazyFunction(z: => Int): Int = {
    z
  }

}
