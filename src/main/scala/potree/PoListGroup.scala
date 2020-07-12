package potree

import cats.kernel.PartialOrder
import cats.kernel.Group

// A will be a Tree here, in practice
class PoListGroup[A](
  po: PartialOrder[A],
  group: Group[A]
) extends Group[List[A]] {
  import cats.syntax.eq._

  def empty = List.empty

  def combine(l: List[A], r: List[A]): List[A] = {
    val rInverse = r.map(group.inverse)
    if (l.intersect(rInverse).nonEmpty) {
      empty
    } else {
      r.foldLeft(l)((c, d) => c.filterNot(po.gt(_, d))).union(
        l.foldLeft(r)((c, d) => c.filterNot(po.gteqv(_, d)))
      )
    }
  }

  // or(1, 2) -> or(and(1, -2), and(2, -1), and(1, 2))

  // or(1, and(2, 3)) -> or(and(1, or(-2, -3)), and(2, 3, -1), and(1, 2, 3))

  // or(1, 2a, 2b) -> or(1, and(2, or(a, b))) how do we achieve this? Factoring

  def inverse(a: List[A]) = a.map(group.inverse)
}
