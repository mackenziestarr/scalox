object State:
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

case class State[S, +A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] = flatMap {
    a => State.unit[S, B](f(a))
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

