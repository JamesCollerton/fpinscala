object RunExample {

	def main(args: Array[String]): Unit = {

		val cafe = new Cafe
		val creditCard = new CreditCard
		val (coffee, charge) = cafe.buyCoffee(creditCard)
		val (coffeeTwo, chargeTwo) = cafe.buyCoffee(creditCard)

		val finalCharge = charge.combine(chargeTwo)
		println(finalCharge.amount)
	}

}
