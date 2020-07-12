package potree

import cats.kernel.Eq
import cats.kernel.BoundedSemilattice

class ListSemilattice[A](
  eq: Eq[A]
) extends BoundedSemilattice[List[A]] {
  import cats.syntax.eq._

  def empty: List[A] = List.empty

  def combine(a: List[A], b: List[A]): List[A] = {
    a.foldLeft(b)((acc, neg) => acc.filterNot(pos => eq.eqv(pos, neg))) ++
    b.foldLeft(a)((acc, neg) => acc.filterNot(pos => eq.eqv(pos, neg)))
  }
}
