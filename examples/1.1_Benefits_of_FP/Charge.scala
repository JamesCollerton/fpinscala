case class Charge(val cc: CreditCard, val amount: Double) {

	def combine(other: Charge): Charge = {
		if(cc == other.cc) {
			Charge(cc, amount + other.amount)
		} else {
			throw new Exception("Cannot combine charges from different cards")
		}
	}

}
