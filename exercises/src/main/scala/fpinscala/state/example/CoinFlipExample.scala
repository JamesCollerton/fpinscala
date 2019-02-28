package fpinscala.state.example

object CoinFlipExample extends App {

  coinflipExample()

  def coinflipExample(): Unit = {

      val initialState = new CoinFlip

      println(CoinFlip.flip(initialState))

      Range(0, 100).foreach(i => if(CoinFlip.nextHead(initialState)._1.equals(CoinResult.Tails)) println("Failed"))
      Range(0, 100).foreach(i => if(CoinFlip.nextTail(initialState)._1.equals(CoinResult.Heads)) println("Failed"))

      println(CoinFlip.pairFlips(initialState))
      println(CoinFlip.listFlips(initialState, 10))

  }

}
