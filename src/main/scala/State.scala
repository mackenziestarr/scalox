object State:
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

case class State[S, +A](run: S => (A, S)):
  def discard = map(_ => ())
  def runS(s: S) = run(s)._2
  def runA(s: S) = run(s)._1
  def map[B](f: A => B): State[S, B] = flatMap {
    a => State.unit[S, B](f(a))
  }
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }