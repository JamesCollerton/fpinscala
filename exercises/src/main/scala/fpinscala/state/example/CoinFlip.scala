package fpinscala.state.example

import fpinscala.state.example.CoinResult.CoinResult

import scala.util.Random

/*
  This is the interface we program to in order to maintain state
 */
class CoinFlip {

  def nextResult: (CoinResult, CoinFlip) = {
    (CoinResult(Random.nextInt(CoinResult.maxId)), this)
  }

}

/*
  This contains all of the methods in order to generate values from the state
 */
object CoinFlip {

  /*
    This type represents a function which goes from a state to another state with a result
   */
  type Flip = CoinFlip => (CoinResult, CoinFlip)

  /*
    This is the same as getting the same result without changing state
   */
  def unit(coinResult: CoinResult): Flip = c => (coinResult, c)

  /*
    This represents flipping a coin once
   */
  def flip(coinFlip: CoinFlip): (CoinResult, CoinFlip) = coinFlip.nextResult

  /*
    This represents flipping a coin until it comes up heads
   */
  def nextHead(coinFlip: CoinFlip): (CoinResult, CoinFlip) = {
    nextRequiredResult(coinFlip, CoinResult.Heads)
  }

  def nextRequiredResult(coinFlip: CoinFlip, requiredResult: CoinResult): (CoinResult, CoinFlip) = {

    /*
      This essentially declares a function which will take a state and then
      continue generating until we get a tail
     */
    def go(s: State[CoinFlip, CoinResult]): State[CoinFlip, CoinResult] = {
      s.flatMap(cr => if(cr.equals(requiredResult)) State.unit(cr) else go(s))
    }

    val flip = State[CoinFlip, CoinResult](c => c.nextResult)

    go(flip).run(coinFlip)

  }

  /*
    This represents flipping a coin until it comes up tails
   */
  def nextTail(coinFlip: CoinFlip): (CoinResult, CoinFlip) = {
    nextRequiredResult(coinFlip, CoinResult.Tails)
  }

  /*
    This is the same as flipping a coin twice
   */
  def pairFlips(coinFlip: CoinFlip): ((CoinResult, CoinResult), CoinFlip) = {

    val (crA, cfA) = coinFlip.nextResult
    val (crB, cfB) = cfA.nextResult

    ((crA, crB), cfB)

  }

  /*
      This generates a list of flips of length n
     */
  def listFlips(coinFlip: CoinFlip, n: Int): (List[CoinResult], CoinFlip) = {

    def go(s: State[CoinFlip, List[CoinResult]], m: Int): State[CoinFlip, List[CoinResult]] = {
      s.flatMap(cr => {
        if(m <= 0) {
          State.unit(cr)
        } else {
          val (newValue, discardState) = coinFlip.nextResult
          go(s.map(l => newValue :: l), m - 1)
        }
      })
    }

    val flip = State[CoinFlip, List[CoinResult]](c => {
      val (cr, s) = c.nextResult
      (List(cr), s)
    })

    go(flip, n).run(coinFlip)
  }

}
