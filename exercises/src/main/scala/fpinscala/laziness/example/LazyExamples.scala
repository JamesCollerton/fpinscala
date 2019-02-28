package fpinscala.laziness.example

object LazyExamples {

  def main(args: Array[String]): Unit = {
    println(lazyArgument(headSupplier))
    println(lazyArgument(tailSupplier))
    println(lazyCons(headSupplier, tailSupplier))
  }

  def headSupplier: Int = {
    println("In head supplier")
    1
  }

  def tailSupplier: List[Int] = {
    println("In tail supplier")
    List(2, 3)
  }

  /*
    Using this we can see that by passing the argument in here we evaluate it without brackets
   */
  def lazyArgument[A](z: => A): A = {
    z
  }

  /*
    Using this we can see that by assigning to the lazy val we don't evaluate until we actually construct
   */
  def lazyCons[A](h: => A, t: => List[A]): List[A] = {
    println("Assigning head")
    lazy val lh = h
    println("Assigning tail")
    lazy val lt = t
    println("Constructing")
    lh :: lt
  }

}
