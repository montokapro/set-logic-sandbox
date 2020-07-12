package potree

sealed abstract trait Dual[A] {
  def inverse: Dual[A]
}

case class Neg[A](value: A) extends Dual[A] {
  def inverse = Pos(value)
}

case class Pos[A](value: A) extends Dual[A] {
  def inverse = Neg(value)
}

class DualInverse[A] extends Inverse[Dual[A]] {
  def inverse(dual: Dual[A]): Dual[A] = dual.inverse
}
