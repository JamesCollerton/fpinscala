case class ExampleClass(val name: String, val age: Int, val weight: Double) {

	def print(): Unit = {
		List(name, age, weight).foreach(println)
	}

}

object ExampleClass {

	def apply: ExampleClass = {
		new ExampleClass("James", 24, 66.5)
	}

}
