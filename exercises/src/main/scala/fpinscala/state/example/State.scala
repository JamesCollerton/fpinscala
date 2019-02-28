package fpinscala.state.example

/*
  This represents the current state of something. We pass it a function which will take us from one state to the
  next
 */
case class State[S, +A](run: S => (A, S)) {

  /*
    We have want a function S => f(A), S
   */
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }

  /*
    We have
      this.run: S => (A, S)
      sb.run: S => (B, S)
      f: (A, B) => C

    We then want:
      S => (C, S)

    So we want to do:

      S => (A, S)
      S => (B, S)

    Then get the results and combine them
   */
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  /*
    We want a function which goes S => (B, S) where we only care that the state is reproducible
   */
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, t) = run(s)
      f(a).run(t)
    })
  }

}

/*
  This is the accompanying object.
 */
object State {

  /*
    If we want to pass along a value without a state
   */
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

}
